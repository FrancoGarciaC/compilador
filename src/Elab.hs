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
-- el primer argumento es el entorno con los sinonimos de tipo
-- el segundo una lista con pares nombre de argumento,tipo
buildType :: [(Name,Ty)]->[(Name,Ty)] -> Ty  
buildType e [(_,t)] = convert e t
buildType e ((_,t):ts) = FunTy (convert e t)  $ buildType e ts 



convert::[(Name,Ty)]->Ty->Ty 
convert e (SinTy n) = case lookup n e of 
                        Just t -> t
                        Nothing -> error "Esto no deberia pasar"
convert e t = t                        


desugar ::[(Name,Ty)]->SDecl STerm -> Decl NTerm
desugar e decl = 
               let  pos = sdeclPos decl
                    name = sdeclName decl
                    body = sdeclBody decl
                    args = sdeclArgs decl
                    info = sgetInfo body
                    typ  = convert e (sdeclType decl)
                    rec = sdeclRec decl 
               in case args of
                    [] -> let body'= desugar' e body                                
                          in (Decl pos name body')
                    otherwise -> if not rec then let body'= desugar' e $ Slam info args body
                                                 in (Decl pos name body')                     
                                 else  case args of 
                                          [(x,tx)] -> let body'= desugar' e $ SFix info name (FunTy tx typ) [(x,tx)] body 
                                                      in (Decl pos name body')                     
                                           
                                          otherwise -> let  tailArgs = tail args
                                                            fun = Slam info tailArgs body
                                                            ls = tailArgs++[("ret",typ)] 
                                                            decl' = SDecl pos rec name [head args] (buildType e ls) fun 
                                                       in desugar e decl'                     
                  




desugar' :: [(Name,Ty)]-> STerm -> NTerm
desugar' e (Sv i v) = V i v
desugar' e (SConst i c) = Const i (desugar'' c)
desugar' e (SPrint i str stm) = Print i str (desugar' e stm)
desugar' e (Slam i [(n, t)] stm) = Lam i n (convert e t) $ desugar' e stm
desugar' e (Slam i ((n,t):ls) stm) = Lam i n (convert e t) $ desugar' e (Slam i ls stm)
desugar' e (SBinaryOp i op stm1 stm2) = BinaryOp i op (desugar' e stm1) (desugar' e stm2)
desugar' e (SApp i stm1 stm2) = App i (desugar' e stm1) (desugar' e stm2) 
desugar' e (SFix i f tr [(n,ty)] t) = Fix i f tr n (convert e ty) (desugar' e t)
desugar' e (SFix i f tr ((n,ty):ns) t ) = let fun = Slam i ns t
                              in error $ show tr
desugar' e (SIfZ i c tt tf) = IfZ i (desugar' e c) (desugar' e tt) (desugar' e tf)
desugar' e (SLet i rec n ls t stmDef stmBody) = 
       let def = desugar' e stmDef
           body = desugar' e stmBody 
           t' = convert e t           
       in case ls of 
            [] -> Let i n  t' def body
            otherwise -> if not rec then Let i n (FunTy (buildType e ls) t') def body
                         else case ls of 
                                [(a,ta)] ->let fix= Fix i n t' a ta def                                               
                                           in Let i n (FunTy ta t') fix body
                                ((a,ta):ts)-> let fun=Slam i ls stmDef 
                                              in desugar' e $ SLet i rec n [(a,ta)] (FunTy (buildType e ts) t') fun stmBody    

desugar''::SConst->Const
desugar'' (SCNat n) = CNat n