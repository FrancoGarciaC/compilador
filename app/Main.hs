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
import Data.List (nub,  intersperse, isPrefixOf )
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
import Elab ( elab,desugar,desugar')
import Eval ( eval )
import PPrint ( pp , ppTy, ppDecl )
import MonadFD4
import TypeChecker ( tc, tcDecl )

prompt :: String
prompt = "FD4> "

{- 
 Tipo para representar las banderas disponibles en línea de comando.
-}
data Mode =
    Interactive
  | Typecheck
  -- | InteractiveCEK
  -- | Bytecompile 
  -- | RunVM
  -- | CC
  -- | Canon
  -- | LLVM
  -- | Build

-- | Parser de banderas
parseMode :: Parser (Mode,Bool)
parseMode = (,) <$>
      (flag' Typecheck ( long "typecheck" <> short 't' <> help "Chequear tipos e imprimir el término")
  -- <|> flag' InteractiveCEK (long "interactiveCEK" <> short 'k' <> help "Ejecutar interactivamente en la CEK")
  -- <|> flag' Bytecompile (long "bytecompile" <> short 'm' <> help "Compilar a la BVM")
  -- <|> flag' RunVM (long "runVM" <> short 'r' <> help "Ejecutar bytecode en la BVM")
      <|> flag Interactive Interactive ( long "interactive" <> short 'i' <> help "Ejecutar en forma interactiva")
  -- <|> flag' CC ( long "cc" <> short 'c' <> help "Compilar a código C")
  -- <|> flag' Canon ( long "canon" <> short 'n' <> help "Imprimir canonicalización")
  -- <|> flag' LLVM ( long "llvm" <> short 'l' <> help "Imprimir LLVM resultante")
  -- <|> flag' Build ( long "build" <> short 'b' <> help "Compilar")
      )
   <*> pure False
   -- reemplazar por la siguiente línea para habilitar opción
   -- <*> flag False True (long "optimize" <> short 'o' <> help "Optimizar código")

-- | Parser de opciones general, consiste de un modo y una lista de archivos a procesar
parseArgs :: Parser (Mode,Bool, [FilePath])
parseArgs = (\(a,b) c -> (a,b,c)) <$> parseMode <*> many (argument str (metavar "FILES..."))


--helper :: Parser (a -> a)
--(<**>) :: Applicative f => f a -> f (a -> b) -> f b
--info :: Parser a -> InfoMod a -> ParserInfo a
--data ParserInfo a: A full description for a runnable Parser for a program.



main :: IO ()
main = execParser opts >>= go
  where
    opts = info (parseArgs <**> helper)
      ( fullDesc
     <> progDesc "Compilador de FD4"
     <> header "Compilador de FD4 de la materia Compiladores 2021" )

    go :: (Mode,Bool,[FilePath]) -> IO ()
    go (Interactive,_,files) =
              do runFD4 (runInputT defaultSettings (repl files))
                 return ()
    go (Typecheck,opt, files) =
              runOrFail $ mapM_ (typecheckFile opt) files
    -- go (InteractiveCEK,_, files) = undefined
    -- go (Bytecompile,_, files) =
    --           runOrFail $ mapM_ bytecompileFile files
    -- go (RunVM,_,files) =
    --           runOrFail $ mapM_ bytecodeRun files
    -- go (CC,_, files) =
    --           runOrFail $ mapM_ ccFile files
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

repl :: (MonadFD4 m, MonadMask m) => [FilePath] -> InputT m ()
repl args = do
       lift $ catchErrors $ compileFiles args
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
                       b <- lift $ catchErrors $ handleCommand c
                       maybe loop (`when` loop) b

compileFiles ::  MonadFD4 m => [FilePath] -> m ()
compileFiles []     = return ()
compileFiles (x:xs) = do
        modify (\s -> s { lfile = x, inter = False })
        compileFile x
        compileFiles xs

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







compileFile ::  MonadFD4 m => FilePath -> m ()
compileFile f = do
    printFD4 ("Abriendo "++f++"...")
    let filename = reverse(dropWhile isSpace (reverse f))
    x <- liftIO $ catch (readFile filename)
               (\e -> do let err = show (e :: IOException)
                         hPutStrLn stderr ("No se pudo abrir el archivo " ++ filename ++ ": " ++ err)
                         return "")
    decls <- parseIO filename program x
    mapM_ handleDecl decls

typecheckFile ::  MonadFD4 m => Bool -> FilePath -> m ()
typecheckFile opt f = do
    printFD4  ("Chequeando "++f)
    decls <- loadFile f
    ppterms <- mapM (typecheckDecl >=> ppDecl) decls
    mapM_ printFD4 ppterms

parseIO ::  MonadFD4 m => String -> P a -> String -> m a
parseIO filename p x = case runP p x filename of
                  Left e  -> throwError (ParseErr e)
                  Right r -> return r

-- 


typecheckDecl :: MonadFD4 m => SDecl STerm -> m (Decl Term)
typecheckDecl decl = do
        let  (Decl _ _ t) = desugar decl
             p = sdeclPos decl
             x = sdeclName decl
             dd = (Decl p x (elab t))
        tcDecl dd
        return dd

-- Nat
-- SinType n
-- Ty->Ty
-- FunTy (SinType n) Ty

handleDecl ::  MonadFD4 m => SDecl STerm -> m ()
handleDecl d@SDecl {} = do
        ty' <- checkSinType $ sdeclType d
        let (args,typs) = unzip $ sdeclArgs d
        typs' <- mapM checkSinType typs
        let d' = d {sdeclArgs =zip args typs'} { sdeclType = ty'}
        (Decl p x tt) <- typecheckDecl d'
        te <- eval tt
        addDecl (Decl p x te)
handleDecl d@SType {} = do
      let n=sinTypeName d
          v=sinTypeVal d
      res <- lookupSinTy n
      case res of
          Just _ -> failFD4 $ "La variable de tipo "++n++" ya fue definida"
          Nothing -> do v'<-checkSinType v
                        addSinType n v'




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
handleCommand ::  MonadFD4 m => Command  -> m Bool
handleCommand cmd = do
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
                          CompileInteractive e -> compilePhrase e
                          CompileFile f        -> put (s {lfile=f, cantDecl=0}) >> compileFile f
                      return True
       Reload ->  eraseLastFileDecls >> (getLastFile >>= compileFile) >> return True
       PPrint e   -> printPhrase e >> return True
       Type e    -> typeCheckPhrase e >> return True

compilePhrase ::  MonadFD4 m => String -> m ()
compilePhrase x =   do
    dot <- parseIO "<interactive>" declOrStm x
    case dot of
      Left d  -> handleDecl d
      Right t -> handleTerm t

handleTerm ::  MonadFD4 m => STerm -> m ()
handleTerm term = do         
         let t = desugar' term
         let tt = elab t
         s <- get
         ty <- tc tt (tyEnv s)
         te <- eval tt
         ppte <- pp te
         printFD4 (ppte ++ " : " ++ ppTy ty)

printPhrase   :: MonadFD4 m => String -> m ()
printPhrase x =
  do
    x' <- parseIO "<interactive>" stm x    
    let ex = elab (desugar' x')
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
         let tt = elab (desugar' t)
         s <- get
         ty <- tc tt (tyEnv s)
         printFD4 (ppTy ty)



