{-|
Module      : Elab
Description : Elabora un término fully named a uno locally closed.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

Este módulo permite elaborar términos y declaraciones para convertirlas desde
fully named (@NTerm) a locally closed (@Term@) 
-}

module Elab ( elab, elab_decl ) where

import Lang
import Subst

-- | 'elab' transforma variables ligadas en índices de de Bruijn
-- en un término dado. 
elab :: NTerm -> Term
elab = elab' []

elab' :: [Name] -> NTerm -> Term
elab' env (V p v) =
  -- Tenemos que hver si la variable es Global o es un nombre local
  -- En env llevamos la lista de nombres locales.
  if v `elem` env 
    then  V p (Free v)
    else V p (Global v)

elab' _ (Const p c) = Const p c
elab' env (Lam p v ty t) = Lam p v ty (close v (elab' (v:env) t))
elab' env (Fix p f fty x xty t) = Fix p f fty x xty (closeN [f, x] (elab' (x:f:env) t))
elab' env (IfZ p c t e)         = IfZ p (elab' env c) (elab' env t) (elab' env e)
-- Operador Print
elab' env (Print i str t) = Print i str (elab' env t)
-- Operadores binarios
elab' env (BinaryOp i o t u) = BinaryOp i o (elab' env t) (elab' env u)
-- Aplicaciones generales
elab' env (App p h a) = App p (elab' env h) (elab' env a)
elab' env (Let p v vty def body) = Let p v vty (elab' env def) (close v (elab' (v:env) body))

elab_decl :: Decl NTerm -> Decl Term
elab_decl = fmap elab


data Decl a = Decl
  { declPos  :: Pos
  , declName :: Name
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

data Ty =
      NatTy
    | FunTy Ty Ty


-- se consideran listas con al menor un argumento
buildType :: [(Name,Ty)] -> Ty  
buildType [(_,t)] = t
buildType ((_,t):ts) = FunTy t $ buildType ts 


SFix info Name Ty [(Name,Ty)] (Stm info var)
Slam info [(Name,Ty)] (Stm info var)
desugar :: SDecl Stm -> Decl NTerm
desugar (SDecl r) = let pos = sdeclPos r
                        name = sdeclName r
                        body = sdeclBody r
                        args = sdeclArgs r
                        info = getInfo body
                        typ  = sdeclType r
                        rec = sdeclRec r 
                        term = case sdeclArgs r of
                                [] -> body                                
                                ls -> if not rec then Slam info args body
                                      else case ls of 
                                              [x] -> SFix info name typ [x] body 
                                              otherwise -> let  tailArgs = tail args
                                                                fun = Slam info tail body  
                                                                dec = SDecl rec pos name (head args) (buildType tailArgs) fun 
                                                          in desugar dec                                                                                            
                        body' = desugar' term
                    in (Decl pos name body')



data Stm info var =     
    Sv info var
  | SConst info var  
  | SPrint info String (Stm info var)
  | Slam info [(Name,Ty)] (Stm info var)
  | SBinaryOp info BinaryOp (Stm info var) (Stm info var)
  | SApp info (Stm info var) (Stm info var) 
  | SLet info Bool Name [(Name,Ty)] Ty (Stm info var)  (Stm info var)
  | SFix info Name Ty [(Name,Ty)] (Stm info var)
  | SIfZ info (Stm info var) (Stm info var) (Stm info var)
  
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
  
desugar' :: Stm -> NTerm
desugar' (Sv i v) = V i v
desugar' (SConst i v) = Const i v
desugar' (SPrint i str stm) = Print i str Stm
desugar' (Slam i [(n,t)] stm) = Lam i n t $ desugar' stm
desugar' (Slam i ((n,t):ls) stm) = Lam i n t $ desugar' (Slam i ls stm)
desugar' (SBinaryOp i op stm1 stm2) = BinaryOp i op (desugar' stm1) (desugar' stm2)
desugar  (SApp i stm1 stm2) = App i (desugar' stm1) (desugar' stm2) 
desugar' (SLet i rec n ls t stmDef stmBody) = 
       let def = desugar' stmDef
           body = desugar' stmBody 
           infoDef = 
       in case ls of 
            [] -> Let i n  t def body
            otherwise -> if not rec then let term = Lam 
           

