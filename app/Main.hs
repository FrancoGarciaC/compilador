{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Main
Description : Compilador de FD4.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

-}

module Main where

import System.Console.Haskeline ( defaultSettings, getInputLine, runInputT, InputT )
import Control.Monad.Catch (MonadMask)

--import Control.Monad
import Control.Monad.Trans
import Data.List (nub,  intersperse, isPrefixOf, null)
import Data.List.Split (endBy)
import Data.Maybe (fromJust,isJust)
import Data.Char ( isSpace )
import Control.Exception ( catch , IOException )
import System.IO ( hPrint, stderr, hPutStrLn )

import System.Exit
--import System.Process ( system )
import Options.Applicative
--import Data.Text.Lazy (unpack)

import Global ( GlEnv(..) )
import Errors
import Lang
import Parse ( P, stm, program, runP,declOrStm)
import Elab ( elab,desugar,desugar', desugarTypeList)
import Eval ( eval )
import PPrint ( pp , ppTy, ppDecl )
import MonadFD4
import TypeChecker ( tc, tcDecl )
import CEK
import System.IO.Extra (FilePath)
import Bytecompile 
import MonadFD4 (failFD4, MonadFD4)
import Optimizations
import IR(IrDecl,IrDecls(..),irDeclName,irDeclArgNames,Ir(..),IrDecl(..))
import ClosureConvert(closureConvert,fromStateToList)
import Control.Monad.Writer.Lazy
import C(ir2C)


prompt :: String
prompt = "FD4> "

{- 
 Tipo para representar las banderas disponibles en línea de comando.
-}
data Mode =
    Interactive
  | Typecheck
  | InteractiveCEK
  | Bytecompile
  | RunVM
  | CC
  -- | Canon
  -- | LLVM
  -- | Build

-- | Parser de banderas
parseMode :: Parser (Mode,Bool)
parseMode = (,) <$>
      (flag' Typecheck ( long "typecheck" <> short 't' <> help "Chequear tipos e imprimir el término")
      <|> flag' InteractiveCEK (long "interactiveCEK" <> short 'k' <> help "Ejecutar interactivamente en la CEK")
      <|> flag' Bytecompile (long "bytecompile" <> short 'm' <> help "Compilar a la BVM")
      <|> flag' RunVM (long "runVM" <> short 'r' <> help "Ejecutar bytecode en la BVM")
      <|> flag Interactive Interactive ( long "interactive" <> short 'i' <> help "Ejecutar en forma interactiva")
      <|> flag' CC ( long "cc" <> short 'c' <> help "Compilar a código C")
  -- <|> flag' Canon ( long "canon" <> short 'n' <> help "Imprimir canonicalización")
  -- <|> flag' LLVM ( long "llvm" <> short 'l' <> help "Imprimir LLVM resultante")
  -- <|> flag' Build ( long "build" <> short 'b' <> help "Compilar")
      )
   -- <*> pure False
   -- reemplazar por la siguiente línea para habilitar opción
   <*> flag False True (long "optimize" <> short 'o' <> help "Optimizar código")

-- | Parser de opciones general, consiste de un modo y una lista de archivos a procesar
parseArgs :: Parser (Mode,Bool, [FilePath])
parseArgs = (\(a,b) c -> (a,b,c)) <$> parseMode <*> many (argument str (metavar "FILES..."))


--helper :: Parser (a -> a)
--(<**>) :: Applicative f => f a -> f (a -> b) -> f b
--info :: Parser a -> InfoMod a -> ParserInfo a
--data ParserInfo a: A full description for a runnable Parser for a program.

data TypeEval = NEval | CEKEval

main :: IO ()
main = execParser opts >>= go
  where
    opts = info (parseArgs <**> helper)
      ( fullDesc
     <> progDesc "Compilador de FD4"
     <> header "Compilador de FD4 de la materia Compiladores 2021" )

    go :: (Mode,Bool,[FilePath]) -> IO ()
    go (Interactive,_,files) =
              do  runFD4 (runInputT defaultSettings (repl NEval files))
                  return ()
    go (Typecheck,opt, files) =
              runOrFail $ mapM_ (typecheckFile opt) files
    go (InteractiveCEK,_, files) = do runFD4 (runInputT defaultSettings (repl CEKEval files))
                                      return ()
    go (Bytecompile,_, files) =
               runOrFail $ mapM_ bytecompileFile files
    go (RunVM,_,files) =
               runOrFail $ mapM_ bytecodeRun files
    go (CC,_, files) =
               runOrFail $ mapM_ ccFile files
    -- go (Canon,_, files) =
    --           runOrFail $ mapM_ canonFile files 
    -- go (LLVM,_, files) =
    --           runOrFail $ mapM_ llvmFile files
    -- go (Build,_, files) =
    --           runOrFail $ mapM_ buildFile files

runOrFail :: FD4 a -> IO a
runOrFail m = do
  r <- runFD4 m
  case r of
    Left err -> do
      liftIO $ hPrint stderr err
      exitWith (ExitFailure 1)
    Right v -> return v

repl :: (MonadFD4 m, MonadMask m) => TypeEval -> [FilePath] -> InputT m ()
repl e args = do
       lift $ catchErrors $ compileFiles e args
       s <- lift get
       when (inter s) $ liftIO $ putStrLn
         (  "Entorno interactivo para FD4.\n"
         ++ "Escriba :? para recibir ayuda.")
       loop
  where loop = do
           minput <- getInputLine prompt
           case minput of
               Nothing -> return ()
               Just "" -> loop
               Just x -> do
                       c <- liftIO $ interpretCommand x
                       b <- lift $ catchErrors $ handleCommand e c
                       maybe loop (`when` loop) b

compileFiles ::  MonadFD4 m => TypeEval -> [FilePath] -> m ()
compileFiles _ []     = return ()
compileFiles e (x:xs) = do
        modify (\s -> s { lfile = x, inter = False })
        compileFile e x
        compileFiles e xs

loadFile ::  MonadFD4 m => FilePath -> m [SDecl STerm]
loadFile f = do
    let filename = reverse(dropWhile isSpace (reverse f))
    x <- liftIO $ catch (readFile filename)
               (\e -> do let err = show (e :: IOException)
                         hPutStrLn stderr ("No se pudo abrir el archivo " ++ filename ++ ": " ++ err)
                         return "")
    setLastFile filename
    ls<-parseIO filename program x
    return ls



compileFile ::  MonadFD4 m => TypeEval -> FilePath -> m ()
compileFile t f = do
    printFD4 ("Abriendo "++f++"...")
    let filename = reverse(dropWhile isSpace (reverse f))
    x <- liftIO $ catch (readFile filename)
               (\e -> do let err = show (e :: IOException)
                         hPutStrLn stderr ("No se pudo abrir el archivo " ++ filename ++ ": " ++ err)
                         return "")
    decls <- parseIO filename program x
    mapM_ (handleDecl t) decls

typecheckFile ::  MonadFD4 m => Bool -> FilePath -> m ()
typecheckFile opt f = do
    printFD4 $"Chequeando "++f
    printFD4 $ "Opt:" ++ show opt
    decls <- loadFile f
    error "falta corregir"
    {- ppterms <- mapM (typecheckDecl >=> (optDeclaration opt).fromJust >=> ppDecl) decls    
    mapM_ printFD4 ppterms -}

parseIO ::  MonadFD4 m => String -> P a -> String -> m a
parseIO filename p x = case runP p x filename of
                  Left e  -> throwError (ParseErr e)
                  Right r -> return r


typecheckDecl :: MonadFD4 m => SDecl STerm -> m (Maybe (Decl TTerm))
typecheckDecl decl@SDecl {} = do
        typeNoSugar <- desugarTypeList $ (snd $ unzip $ sdeclArgs decl) ++  [sdeclType decl]
        (Decl p n t) <- desugar decl       
        let dd = (Decl p n (elab t))   
        tterm <- tcDecl typeNoSugar dd
        let dd' = Decl p n tterm
        return $ Just dd'

typecheckDecl d@SType {} = do
      let n = sinTypeName d
          v = sinTypeVal d
      res <- lookupSinTy n
      case res of
          Just _ -> failFD4 $ "La variable de tipo "++n++" ya fue definida"
          Nothing -> do v'<-desugarType v                        
                        addSinType n v'
                        return Nothing



bytecompileFile :: MonadFD4 m => FilePath -> m ()
bytecompileFile filePath = do ds <- loadFile filePath
                              ds' <- mapM typecheckDecl ds >>= \xs -> return $ map fromJust $ filter isJust xs
                              bc <- bytecompileModule ds'                              
                              case endBy ".fd4" filePath of 
                                [path] -> liftIO $ bcWrite bc $ path ++ ".byte"
                                _ -> failFD4 "Error: el archivo debe tener extension .fd4"

bytecodeRun :: MonadFD4 m => FilePath -> m()
bytecodeRun filePath = do bc <- liftIO $ bcRead filePath
                          runBC bc
                              



handleDecl ::  MonadFD4 m => TypeEval -> SDecl STerm -> m ()
handleDecl t d@SDecl {} = do
        ty' <- desugarType $ sdeclType d
        let (args,typs) = unzip $ sdeclArgs d
        typs' <- mapM desugarType typs
        let d' = d {sdeclArgs =zip args typs'} { sdeclType = ty'}
        error "falta corregir"
        {- (Decl p x tt) <- typecheckDecl d' >>= \d -> return $ fromJust d
        te <- runEval t tt
        addDecl (Decl p x te) -}
handleDecl _ d@SType {} = typecheckDecl d >> return ()

data Command = Compile CompileForm
             | PPrint String
             | Type String
             | Reload
             | Browse
             | Quit
             | Help
             | Noop

data CompileForm = CompileInteractive  String
                 | CompileFile         String

data InteractiveCommand = Cmd [String] String (String -> Command) String

-- | Parser simple de comando interactivos
interpretCommand :: String -> IO Command
interpretCommand x
  =  if isPrefixOf ":" x then
       do  let  (cmd,t')  =  break isSpace x
                t         =  dropWhile isSpace t'
           --  find matching commands
           let  matching  =  filter (\ (Cmd cs _ _ _) -> any (isPrefixOf cmd) cs) commands
           case matching of
             []  ->  do  putStrLn ("Comando desconocido `" ++ cmd ++ "'. Escriba :? para recibir ayuda.")
                         return Noop
             [Cmd _ _ f _]
                 ->  do  return (f t)
             _   ->  do  putStrLn ("Comando ambigüo, podría ser " ++
                                   concat (intersperse ", " [ head cs | Cmd cs _ _ _ <- matching ]) ++ ".")
                         return Noop

     else
       return (Compile (CompileInteractive x))

commands :: [InteractiveCommand]
commands
  =  [ Cmd [":browse"]      ""        (const Browse) "Ver los nombres en scope",
       Cmd [":load"]        "<file>"  (Compile . CompileFile)
                                                     "Cargar un programa desde un archivo",
       Cmd [":print"]       "<exp>"   PPrint          "Imprime un término y sus ASTs sin evaluarlo",
       Cmd [":reload"]      ""        (const Reload)         "Vuelve a cargar el último archivo cargado",
       Cmd [":type"]        "<exp>"   Type           "Chequea el tipo de una expresión",
       Cmd [":quit",":Q"]        ""        (const Quit)   "Salir del intérprete",
       Cmd [":help",":?"]   ""        (const Help)   "Mostrar esta lista de comandos" ]

helpTxt :: [InteractiveCommand] -> String
helpTxt cs
  =  "Lista de comandos:  Cualquier comando puede ser abreviado a :c donde\n" ++
     "c es el primer caracter del nombre completo.\n\n" ++
     "<expr>                  evaluar la expresión\n" ++
     "let <var> = <expr>      definir una variable\n" ++
     unlines (map (\ (Cmd c a _ d) ->
                   let  ct = concat (intersperse ", " (map (++ if null a then "" else " " ++ a) c))
                   in   ct ++ replicate ((24 - length ct) `max` 2) ' ' ++ d) cs)

-- | 'handleCommand' interpreta un comando y devuelve un booleano
-- indicando si se debe salir del programa o no.
handleCommand ::  MonadFD4 m => TypeEval -> Command  -> m Bool
handleCommand t cmd = do
   s@GlEnv {..} <- get
   case cmd of
       Quit   ->  return False
       Noop   ->  return True
       Help   ->  printFD4 (helpTxt commands) >> return True
       Browse ->  do  printFD4 (unlines [ name | name <- reverse (nub (map declName glb)) ])
                      return True
       -- frases por consola y compilacion de archivos               
       Compile c ->
                  do  case c of
                          CompileInteractive e -> compilePhrase t e
                          CompileFile f        -> put (s {lfile=f, cantDecl=0}) >> compileFile t f
                      return True
       Reload ->  eraseLastFileDecls >> (getLastFile >>= compileFile t) >> return True
       PPrint e   -> printPhrase e >> return True
       Type e    -> typeCheckPhrase e >> return True

compilePhrase ::  MonadFD4 m => TypeEval -> String -> m ()
compilePhrase e x =   do
    dot <- parseIO "<interactive>" declOrStm x
    case dot of
      Left d  -> handleDecl e d
      Right t -> handleTerm e t

handleTerm ::  MonadFD4 m => TypeEval -> STerm -> m ()
handleTerm e term = do
         t <- desugar' term
         let tt = elab t
         s <- get
         tt' <- tc tt (tyEnv s)
         let ty = getInfo tt'
         te <- runEval e tt
         ppte <- pp te
         printFD4 (ppte ++ " : " ++ ppTy ty)

printPhrase   :: MonadFD4 m => String -> m ()
printPhrase x =
  do
    x' <- parseIO "<interactive>" stm x
    tt <- desugar' x'
    let ex = elab tt
    t  <- case x' of
           (Sv p f) -> maybe ex id <$> lookupDecl f
           _       -> return ex
    printFD4 "NTerm:"
    printFD4 (show x')
    printFD4 "\nTerm:"
    printFD4 (show t)

typeCheckPhrase :: MonadFD4 m => String -> m ()
typeCheckPhrase x = do
         t <- parseIO "<interactive>" stm x
         e <- getSinTypEnv
         t' <- desugar' t
         let tt = elab t'
         s <- get         
         tt' <- tc tt (tyEnv s)        
         let ty = getInfo tt'
         printFD4 (ppTy ty)


runEval :: MonadFD4 m => TypeEval -> Term -> m Term
runEval NEval t = eval t
runEval CEKEval t = do  d <- search t [] []
                        return $ fromValtoTerm d

ccFile :: MonadFD4 m => FilePath -> m()
ccFile filePath = do 
  case endBy ".fd4" filePath of 
    [path] -> do ds <- loadFile filePath
                 ds' <- mapM typecheckDecl ds >>= \xs -> return $ map fromJust $ filter isJust xs

                 -- filtrar sinonimos de tipo
                 let ds2 = filter isSugarDecl ds

                 -- mapear cada declaracion con su tipo                  
                 typeMap <- mapM (\d -> let argsTypes = snd $ unzip $ sdeclArgs d in 
                                        desugarTypeList (argsTypes ++ [sdeclType d]) >>= \t ->
                                        return (sdeclName d,t) ) ds2
                 
                 let   -- definir cuales son funciones sin argumentos explicitos
                       funcWithoutArgs = filter (\d ->  isFuncWithoutArgs d typeMap) ds2 

                       funcNamesWithoutArgs = map (\d -> sdeclName d) funcWithoutArgs          
                       info = map (\d ->  let declName = sdeclName d in
                                        (declName,
                                         (checkIfIsVal $ fromJust $ lookup declName typeMap,
                                        sdeclArgs d))
                                        ) ds2              

                       decls = concat $ map (\d ->  fromStateToList d 
                                                                    (fromJust $ lookup (declName d) info)
                                                                    funcNamesWithoutArgs
                                                                     ) ds'   

                       decls' = IrDecls decls   
             
                 liftIO $ writeFile (path ++ ".c") (ir2C decls')
    _ -> failFD4 "Error: el archivo debe tener extension .fd4" 


checkIfIsVal :: Ty -> Bool
checkIfIsVal NatTy  = True
checkIfIsVal _  = False

type TyMap = [(Name,Ty)]

isFuncWithoutArgs :: SDecl STerm -> TyMap -> Bool          
isFuncWithoutArgs d m =  let n = sdeclName d in 
                         if checkIfIsVal (fromJust $ lookup n m) then False
                         else null $ sdeclArgs d              



isSugarDecl SDecl {} = True   
isSugarDecl _ = False

  
