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


bc :: MonadFD4 m => Term -> m Bytecode
bc (V _ (Bound i)) = return [ACCESS,i]
bc (Const _ (CNat n)) = return [CONST,n]
bc (Lam _ n ty t) = do  bcode <- bc t
                        return $ [FUNCTION,length bcode]++bcode++[RETURN]
bc (App _ t1 t2) =  do  bc1 <- bc t1
                        bc2 <- bc t2
                        return $ bc1++bc2++[CALL]
bc (Print _ s t) = do t' <- bc t
                      return $ [PRINT] ++ map ord s ++ [NULL] ++ t' ++ [PRINTN]

bc (BinaryOp _ op t1 t2) = do bc1 <- bc t1
                              bc2 <- bc t2
                              return $ bc1++bc2++[bcOp op]

 where  bcOp Add = ADD
        bcOp Sub = SUB

bc (IfZ _ z t1 t2) = do bcz <- bc z
                        bc1 <- bc t1
                        bc2 <- bc t2
                        return $ bcz ++ bc1 ++ bc2 ++ [IFZ]


bc (Let _ _ ty t1 t2) = do  bc1 <- bc t1
                            bc2 <- bc t2
                            return $ bc1 ++ [SHIFT] ++ bc2 ++ [DROP]

bc (Fix _ _ _ _ _ t) = do bc1 <- bc t
                          return $ [FUNCTION,length bc1]++bc1++[RETURN,FIX]

bc _ = error ""


type Module = [Decl Term]

bytecompileModule :: MonadFD4 m => Module -> m Bytecode
bytecompileModule ds = do let t = elabTerm ds
                          bc t

-- Construye un término Let a partir de una lista de declaraciones
-- cerrando las variables libres en el cuerpo de cada declaracion 
-- y convirtiendo variables globales en libres
elabTerm :: [Decl Term] -> Term
elabTerm [Decl _ _ body] = replaceGlobal body
elabTerm ((Decl pos name body):xs) = Let pos name NatTy (replaceGlobal body)  $ close name (elabTerm xs)
elabTerm [] = error "No debería pasar"


-- Cambiar todas las variables globales por libres en el término
replaceGlobal::Term -> Term
replaceGlobal = fmap changeGlobal
  where changeGlobal (Global n) = Free n
        changeGlobal v = v
  

{-replaceGlobal (V i (Global n)) = V i (Free n)
replaceGlobal (Lam i n ty t) = Lam i n ty (replaceGlobal t)
replaceGlobal (App i t1 t2) = App i (replaceGlobal t1) (replaceGlobal t2)
replaceGlobal (Print i s t) = Print i s (replaceGlobal t)
replaceGlobal (BinaryOp i op t1 t2) = BinaryOp i op (replaceGlobal t1) (replaceGlobal t2)
replaceGlobal (IfZ i t t1 t2) = IfZ i (replaceGlobal t) (replaceGlobal t1) (replaceGlobal t2)
replaceGlobal (Fix i n ty nv tyn t) = Fix i n ty nv tyn (replaceGlobal t)
replaceGlobal (Let i n ty t1  t2) = Let i n ty (replaceGlobal t1) (replaceGlobal t2)
replaceGlobal q = q-}



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

runBC' (CONST:n:c) e s = runBC' c e (I n:s)

runBC' (ACCESS:i:c) e s = runBC' c e (e!!i:s)

runBC' (ADD:c) e (I n1:I n2:s) = runBC' c e (I (n1+n2) :s)

runBC' (SUB:c) e (I n1:I n2:s) = runBC' c e (I (n1-n2) :s)

runBC' (CALL:c) e (v:Fun ef cf :s)  = runBC' cf (v:ef) (RA e c:s)

--runBC' (bcz:bc1:bc2:IFZ:c) e s  = runBC' c e (:s)

runBC' (FUNCTION:ctos:c) e (n1:n2:s) = runBC' (drop ctos c) e (Fun e c:s)

runBC' (RETURN:_) _ (v:RA e c:s) = runBC' c e (v:s)


runBC' _ _ _ = error "No esta contemplado"