{-# LANGUAGE PatternSynonyms #-}
{-|
Module      : Byecompile
Description : Compila a bytecode. Ejecuta bytecode.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental
Este módulo permite compilar módulos a la BVM. También provee una implementación de la BVM 
para ejecutar bytecode.
-}
module Bytecompile
  (Bytecode, runBC, bcWrite, bcRead,bytecompileModule)
 where

import Lang
import Subst
import MonadFD4

import qualified Data.ByteString.Lazy as BS
import Data.Binary ( Word32, Binary(put, get), decode, encode )
import Data.Binary.Put ( putWord32le )
import Data.Binary.Get ( getWord32le, isEmpty )

import Data.Char
import Control.Monad.State.Lazy (Monad)

type Opcode = Int
type Bytecode = [Int]

newtype Bytecode32 = BC { un32 :: [Word32] }

{- Esta instancia explica como codificar y decodificar Bytecode de 32 bits -}
instance Binary Bytecode32 where
  put (BC bs) = mapM_ putWord32le bs
  get = go
    where go =
           do
            empty <- isEmpty
            if empty
              then return $ BC []
              else do x <- getWord32le
                      BC xs <- go
                      return $ BC (x:xs)

{- Estos sinónimos de patrón nos permiten escribir y hacer
pattern-matching sobre el nombre de la operación en lugar del código
entero, por ejemplo:
 
   f (CALL : cs) = ...
 Notar que si hubieramos escrito algo como
   call = 5
 no podríamos hacer pattern-matching con `call`.
 En lo posible, usar estos códigos exactos para poder ejectutar un
 mismo bytecode compilado en distintas implementaciones de la máquina.
-}
pattern NULL     = 0
pattern RETURN   = 1
pattern CONST    = 2
pattern ACCESS   = 3
pattern FUNCTION = 4
pattern CALL     = 5
pattern ADD      = 6
pattern SUB      = 7
pattern IFZ      = 8
pattern FIX      = 9
pattern STOP     = 10
pattern SHIFT    = 11
pattern DROP     = 12
pattern PRINT    = 13
pattern PRINTN   = 14
pattern IFSTOP   = 17
pattern JUMP = 18


bc :: MonadFD4 m => Term -> m Bytecode
bc (V _ (Bound i)) = do printFD4 "Estoy en V"
                        return [ACCESS,i]
bc (Const _ (CNat n)) = do printFD4 "Estoy en Const"
                           return [CONST,n]
bc (Lam _ n ty t) = do printFD4 "Estoy en Lam"
                       bcode <- bc t
                       return $ [FUNCTION,length bcode +1]++bcode++[RETURN]

bc (App _ t1 t2) =  do printFD4 "Estoy en App"
                       bc1 <- bc t1
                       bc2 <- bc t2
                       return $ bc1++bc2++[CALL]
bc (Print _ s t) = do printFD4 "Estoy en Print"
                      t' <- bc t
                      return $ [PRINT] ++ map ord s ++ [NULL] ++ t' ++ [PRINTN]

bc (BinaryOp _ op t1 t2) = do printFD4 "Estoy en BinaryOP"
                              bc1 <- bc t1
                              bc2 <- bc t2
                              return $ bc1++bc2++[bcOp op]

 where  bcOp Add = ADD
        bcOp Sub = SUB

bc (IfZ _ z t1 t2) = do printFD4 "Estoy en IfZ"
                        bcz <- bc z
                        bc1 <- bc t1
                        bc2 <- bc t2
                        return $ [IFZ,length bc1 + 2] ++ bcz ++ [IFSTOP] ++ bc1 ++ [JUMP,length bc2] ++ bc2

bc (Let _ _ ty t1 t2) = do printFD4 "Estoy en Let"
                           bc1 <- bc t1
                           bc2 <- bc t2
                           return $ bc1 ++ [SHIFT] ++ bc2 ++ [DROP]

bc (Fix _ _ _ _ _ t) = do printFD4 "Estoy en Fix"
                          bc1 <- bc t
                          return $ [FUNCTION,length bc1 +1]++bc1++[RETURN,FIX]

bc _ = error "Estoy en bc, esto no debería pasar"


type Module = [Decl Term]

bytecompileModule :: MonadFD4 m => Module -> m Bytecode
bytecompileModule ds = do let t = elabTerm ds
                          do b <- bc t
                             return $ b++[STOP]

-- Construye un término Let a partir de una lista de declaraciones
-- cerrando las variables libres en el cuerpo de cada declaracion 
-- y convirtiendo variables globales en libres
elabTerm :: [Decl Term] -> Term
elabTerm [Decl _ _ body] = replaceGlobal body
elabTerm ((Decl pos name body):xs) = Let pos name NatTy (replaceGlobal body)  $ close name (elabTerm xs)
elabTerm [] = error "No debería pasar"
--elabTerm xs = error $ show xs

-- Cambiar todas las variables globales por libres en el término
replaceGlobal::Term -> Term
replaceGlobal = fmap changeGlobal
  where changeGlobal (Global n) = Free n
        changeGlobal v = v

-- | Toma un bytecode, lo codifica y lo escribe un archivo 
bcWrite :: Bytecode -> FilePath -> IO ()
bcWrite bs filename = do  print $ show bs
                          print $ show (encode $ BC $ fromIntegral <$> bs)
                          BS.writeFile filename (encode $ BC $ fromIntegral <$> bs)

---------------------------
-- * Ejecución de bytecode
---------------------------

-- Tipo de datos para la BVM  

type Env = [Val]
type Stack = [Val]
data Val = I Int | Fun Env Bytecode | RA Env Bytecode  deriving Show



data Closure = ClosFun Env Name Ty Term | ClosFix Env Name Ty Name Ty Term  deriving Show



-- | Lee de un archivo y lo decodifica a bytecode
bcRead :: FilePath -> IO Bytecode
bcRead filename = map fromIntegral <$> un32  <$> decode <$> BS.readFile filename


runBC :: MonadFD4 m => Bytecode -> m ()
runBC c = runBC' c [] []

runBC' :: MonadFD4 m => Bytecode -> Env -> Stack -> m()

runBC' (CONST:n:c) e s = do printFD4 "Estoy en CONST"
                            printFD4 $ "Const:"++show n
                            runBC' c e (I n:s)

runBC' (ACCESS:i:c) e s = do printFD4 "Estoy en ACCESS"
                             printFD4 $ "Index:"++show i
                             case e!!i of
                               I i -> printFD4 $ "Con v :" ++ show i
                               _ -> printFD4 "Tengo fun"
                             runBC' c e (e!!i:s)

runBC' (ADD:c) e (I n1:I n2:s) = do printFD4 "Estoy en ADD"
                                    printFD4 $ "n1:"++show n1++" n2:"++show n2
                                    runBC' c e (I (n1+n2) :s)

runBC' (SUB:c) e (I n1:I n2:s) = do printFD4 "Estoy en SUB x="
                                    printFD4 $ "n1:"++show n1++" n2:"++show n2
                                    let  res = max (n2-n1) 0
                                    runBC' c e (I res :s)

runBC' (CALL:c) e (v:Fun ef cf :s)  = do printFD4 "Estoy en CALL"
                                         printFD4 $ "Con valor " ++ show v
                                         runBC' cf (v:ef) (RA e c:s)

runBC' (IFZ:ctos:c) e s  =
          do printFD4 "Estoy en IFZ"             
             runBC' c e (RA e []:I ctos:s)

runBC' (IFSTOP:c) _ (I k:RA e _:I ctos:s) | k == 0 = do printFD4 "Estoy en caso = 0"
                                                        runBC' c e s
                                          | otherwise = do printFD4 $ "Estoy en caso != 0 "++show c                                                                                                  
                                                           runBC' (drop ctos c) e s

runBC' (JUMP:n:c) e s = runBC' (drop n c) e s                                                           

runBC' (FUNCTION:ctos:c) e s = do printFD4 "Estoy en FUNCTION"
                                  runBC' (drop ctos c) e (Fun e c:s)

runBC' (RETURN:_) _ (v:RA e c:s) = do printFD4 "Estoy en RETURN "                                      
                                      runBC' c e (v:s)

runBC' (FIX:c) e (Fun _ cf:s) = do printFD4 "Estoy en FIX"
                                   let efix = Fun efix cf:e
                                   runBC' c e (Fun efix cf:s)


runBC' (PRINTN:c) e q@(I n:s) = do printFD4 "Estoy en PRINTN"
                                   printFD4 $ show n
                                   runBC' c e q

runBC' (PRINT:c) e s = do printFD4 "Estoy en PRINT"
                          let (str,c') = decode "" c
                          printFD42 str
                          runBC'  c' e s



  where decode s (NULL:c) = (s,c)
        decode s (x:xs) = decode (s++[chr x]) xs
        decode _ [] = error "111 Esto no debería pasar"



runBC' (SHIFT:c) e (v:s) = do printFD4 "Estoy en SHIFT"
                              runBC' c (v:e) s

runBC' (DROP:c) (v:e) s = do printFD4 "Estoy en DROP"
                             runBC' c e s

runBC' (STOP:_) _ _ = do printFD4 "Estoy en STOP"
                         return ()

runBC' xs e s = do printFD4 "Esto no deberia pasar"
                   error $ "BC:"++show xs++"\n"++
                           "Entorno:" ++ show e ++ "\n"++
                           "Stack:" ++ show s

