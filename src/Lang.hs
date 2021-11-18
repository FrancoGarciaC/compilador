{-# LANGUAGE DeriveFunctor #-}

{-|
Module      : Lang
Description : AST de términos, declaraciones y tipos
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

Definiciones de distintos tipos de datos:
  - AST de términos
  - Declaraciones
  - Tipos
  - Variables

-}

module Lang where

import           Common                         ( Pos )
import           Data.List.Extra                ( nubSort )

-- | AST de Tipos
data Ty =
      NatTy
    | FunTy Ty Ty
    | SinTy Name
    deriving (Show,Eq)

type Name = String

data Const = CNat Int
  deriving Show

data SConst = SCNat Int
  deriving Show


data BinaryOp = Add | Sub
  deriving Show


-- Tipo de datos para maquina CEK  
--Valores
data Val = N Int | ClosFun Env Int Term | ClosFix Env Name Int Term | 
--Frames
fr ::= ρ ·  t
| clos
| ρ · ifz  then t else e
| ρ ·  ⊕ t
| v ⊕
| print str 


data Frame info  = FrAp info Env Term 
           | FrClos info Closure
           | FrOpTer info Env Op Term
           | FrOpVal info Op Term
           | FrPrint info String
           | FrIf info Env Term 
-- Continuaciones
data Kont = [Frame]

-- | tipo de datos de declaraciones, parametrizado por el tipo del cuerpo de la declaración
data Decl a = Decl
  { declPos  :: Pos
  , declName :: Name
  --, declType :: Ty
  , declBody :: a
  }
  deriving (Show, Functor)

data SDecl a = SDecl
  { sdeclPos  :: Pos
  , sdeclRec  :: Bool
  , sdeclName :: Name
  , sdeclArgs :: [(Name,Ty)]  
  , sdeclType :: Ty
  , sdeclBody :: a
  }
  | SType { sinTypeName::Name
          , sinTypeVal::Ty}
  deriving (Show, Functor)


-- | AST de los términos. 
--   - info es información extra que puede llevar cada nodo. 
--       Por ahora solo la usamos para guardar posiciones en el código fuente.
--   - var es el tipo de la variables. Es 'Name' para fully named y 'Var' para locally closed.


data Stm info var =     
    Sv info var
  | SConst info SConst  
  | SPrint info String (Stm info var)
  | Slam info [(Name,Ty)] (Stm info var)
  | SBinaryOp info BinaryOp (Stm info var) (Stm info var)
  | SApp info (Stm info var) (Stm info var) 
  | SLet info Bool Name [(Name,Ty)] Ty (Stm info var)  (Stm info var)
  | SFix info Name Ty [(Name,Ty)] (Stm info var)
  | SIfZ info (Stm info var) (Stm info var) (Stm info var)
  deriving (Show,Functor)
--let prueba () : = let f () = 
  


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
  
  deriving (Show, Functor)





type STerm = Stm Pos Name  -- Terminos con sintactic sugar
type NTerm = Tm Pos Name   -- ^ 'Tm' tiene 'Name's como variables ligadas y libres y globales, guarda posición
type Term = Tm Pos Var     -- ^ 'Tm' con índices de De Bruijn como variables ligadas, y nombres para libres y globales, guarda posición

data Var =
    Bound !Int
  | Free Name
  | Global Name
  deriving Show




-- | Obtiene la info en la raíz del término.
getInfo :: Tm info var -> info
getInfo (V     i _       ) = i
getInfo (Const i _       ) = i
getInfo (Lam i _ _ _     ) = i
getInfo (App   i _ _     ) = i
getInfo (Print i _ _     ) = i
getInfo (Fix i _ _ _ _ _ ) = i
getInfo (IfZ i _ _ _     ) = i
getInfo (Let i _ _ _ _   ) = i
getInfo (BinaryOp i _ _ _) = i



-- | Obtiene la info en la raíz del término.
sgetInfo :: Stm info var -> info
sgetInfo (Sv     i _       ) = i
sgetInfo (SConst i _       ) = i
sgetInfo (Slam i _ _     ) = i
sgetInfo (SApp   i _ _     ) = i
sgetInfo (SPrint i _ _     ) = i
sgetInfo (SFix i _ _ _ _ ) = i
sgetInfo (SIfZ i _ _ _     ) = i
sgetInfo (SLet i _ _ _ _ _ _) = i
sgetInfo (SBinaryOp i _ _ _) = i


-- | Obtiene los nombres de variables (abiertas o globales) de un término.
freeVars :: Tm info Var -> [Name]
freeVars tm = nubSort $ go tm [] where
  go (V _ (Free   v)  ) xs = v : xs
  go (V _ (Global v)  ) xs = v : xs
  go (V _ _           ) xs = xs
  go (Lam _ _ _ t     ) xs = go t xs
  go (App   _ l r     ) xs = go l $ go r xs
  go (Print _ _ t     ) xs = go t xs
  go (BinaryOp _ _ t u) xs = go t $ go u xs
  go (Fix _ _ _ _ _ t ) xs = go t xs
  go (IfZ _ c t e     ) xs = go c $ go t $ go e xs
  go (Const _ _       ) xs = xs
  go (Let _ _ _ e t   ) xs = go e (go t xs)
