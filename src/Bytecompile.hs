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



{-
data Tm info var =
    V info var
  | Const info Const
  | Lam info Name Ty (Tm info var)
  | App info (Tm info var) (Tm info var)
  | Print info String (Tm info var)
  | BinaryOp info BinaryOp (Tm info var) (Tm info var)
  | Fix info Name Ty Name Ty (Tm info var)
  | IfZ info (Tm info var) (Tm info var) (Tm info var)
  | Let info Name Ty (Tm info var)  (Tm info var)
  
  deriving (Show, Functor)-}

bc :: MonadFD4 m => Term -> m Bytecode
V _
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
                          return $ [FUNCTION,lengh bc1]++bc1++[RETURN,FIX]

bc _ = error ""


type Module = [Decl Term]
{-
data Decl a = Decl
  { declPos  :: Pos
  , declName :: Name
  , declBody :: a
  }-}

  -- | Let info Name Ty (Tm info var)  (Tm info var)

bytecompileModule :: MonadFD4 m => Module -> m Bytecode
bytecompileModule ds = do let t = elabTerm ds
                          bc t


elabTerm :: [Decl Term] -> Term
elabTerm [Decl pos name body] = replaceGlobal body

elabTerm ((Decl pos name body):xs) = Let pos name NatTy (replaceGlobal body)  $ close name (elabTerm xs)
-- Falta reemplazar Global por Free en 


replaceGlobal :: Term -> Term



-- | Toma un bytecode, lo codifica y lo escribe un archivo 
bcWrite :: Bytecode -> FilePath -> IO ()
bcWrite bs filename = BS.writeFile filename (encode $ BC $ fromIntegral <$> bs)

---------------------------
-- * Ejecución de bytecode
---------------------------

-- | Lee de un archivo y lo decodifica a bytecode
bcRead :: FilePath -> IO Bytecode
bcRead filename = map fromIntegral <$> un32  <$> decode <$> BS.readFile filename

runBC :: MonadFD4 m => Bytecode -> m ()
runBC c = error "implementame"