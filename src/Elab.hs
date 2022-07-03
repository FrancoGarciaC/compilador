{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
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

module Elab ( elab, elab_decl,desugar,desugar',buildType) where

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


-- se consideran listas con al menos un argumento
buildType :: [(Name,Ty)] -> Ty
buildType [(_,t)] = t
buildType ((_,t):ts) = FunTy t  $ buildType ts



-- convert::[(Name,Ty)]->Ty->Ty 
-- convert e (SinTy n) = case lookup n e of 
--                         Just t -> t
--                         Nothing -> error "Esto no deberia pasar"
-- convert e (FunTy t ty) = FunTy (convert e t) (convert e ty)
-- convert e NatTy  = NatTy                         



desugar ::SDecl STerm ->  Decl NTerm
desugar decl =
               let  pos = sdeclPos decl
                    name = sdeclName decl
                    body = sdeclBody decl
                    args = sdeclArgs decl
                    info = sgetInfo body
                    typ  = sdeclType decl
                    rec = sdeclRec decl
               in  case args of
                    [] -> let body'= desugar' body
                          in Decl pos name body'
                    _ -> if not rec then let body'= desugar' $ Slam info args body
                                         in Decl pos name body'
                                 else  case args of
                                          [(x,tx)] -> let body'= desugar' $ SFix info name (FunTy tx typ) [(x,tx)] body
                                                      in Decl pos name body'

                                          _ -> let  tailArgs = tail args
                                                    fun = Slam info tailArgs body
                                                    ls = tailArgs++[("ret",typ)]
                                                    decl' = SDecl pos rec name [head args] (buildType ls) fun
                                               in desugar decl'





desugar' :: STerm -> NTerm
desugar' (Sv i v) = V i v
desugar' (SConst i c) = Const i (desugar'' c)
desugar' (SPrint i str stm) = Print i str (desugar' stm)
desugar' (Slam i [(n, t)] stm) = Lam i n t $ desugar' stm
desugar' (Slam i ((n,t):ls) stm) = Lam i n t $ desugar' (Slam i ls stm)
desugar' (SBinaryOp i op stm1 stm2) = BinaryOp i op (desugar' stm1) (desugar' stm2)
desugar' (SApp i stm1 stm2) = App i (desugar' stm1) (desugar' stm2)
desugar' (SFix i f tr [(n,ty)] t) = Fix i f tr n ty (desugar' t)
desugar' (SFix i f tr ((n,ty):ns) t ) = let fun = Slam i ns t
                              in error $ show tr
desugar' (SIfZ i c tt tf) = IfZ i (desugar' c) (desugar' tt) (desugar' tf)
desugar' q@(SLet i rec n ls t stmDef stmBody) =
       let def = desugar' stmDef
           body = desugar' stmBody
       in case ls of
            [] -> Let i n  t def body
            _ -> if not rec then let fun = Slam i ls stmDef
                                 in Let i n (FunTy (buildType ls) t) (desugar' fun) body
                 else case ls of
                         [(a,ta)] ->let fix= Fix i n t a ta def
                                    in Let i n (FunTy ta t) fix body
                         ((a,ta):ts)->  let fun=Slam i ls stmDef
                                        in desugar' $ SLet i rec n [(a,ta)] (FunTy (buildType ts) t) fun stmBody

desugar''::SConst->Const
desugar'' (SCNat n) = CNat n