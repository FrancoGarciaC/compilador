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

module Elab ( elab, elab_decl,desugar,desugar',desugarTypeList) where

import Lang
import Subst
import MonadFD4


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


-- desugarea sinonimos de tipo
desugarTypeList :: MonadFD4 m => [Ty] -> m Ty
desugarTypeList [t] = desugarType t
desugarTypeList (t:ts) = do t' <- desugarType t 
                            ts' <- desugarTypeList ts    
                            return $ FunTy t' ts'

desugar :: MonadFD4 m => SDecl STerm -> m (Decl NTerm)
desugar decl = let  pos = sdeclPos decl
                    name = sdeclName decl
                    body = sdeclBody decl
                    args = sdeclArgs decl
                    info = sgetInfo body
                    typ  = sdeclType decl
                    rec = sdeclRec decl
               in  case args of
                    [] -> do body' <- desugar' body
                             return $ Decl pos name body'
                    _ -> if not rec then do let slam = Slam info args body
                                            lam <- desugar' slam
                                            return $ Decl pos name lam
                         
                         else do  typNoSugar <- desugarTypeList $ (snd $ unzip args) ++ [typ]
                                  let (v,tv) = head args                                  
                                      sfix = SFix pos name typNoSugar args body
                                  fix <- desugar' sfix
                                  return $ Decl pos name fix                                                                                 


desugar' :: MonadFD4 m => STerm -> m NTerm
desugar' (Sv i v) = return $ V i v

desugar' (SConst i c) = return $ Const i $ desugar'' c

desugar' (SPrint i str stm) =  do stm' <- desugar' stm
                                  return $ Print i str stm'

desugar' (Slam i [(n, st)] stm) = do t <- desugarType st  
                                     stm' <- desugar' stm
                                     return $ Lam i n t stm'

desugar' (Slam i ((n,st):ls) stm) = do stm' <- desugar' $ Slam i ls stm
                                       t <- desugarType st
                                       return $ Lam i n t stm'

desugar' (SBinaryOp i op stm1 stm2) = do stm1' <- desugar' stm1
                                         stm2' <- desugar' stm2
                                         return $ BinaryOp i op stm1' stm2'

                                   
desugar' (SApp i stm1 stm2) = do stm1' <- desugar' stm1 
                                 stm2' <- desugar' stm2
                                 return $ App i stm1' stm2'

desugar' (SFix i f tf ((v,tv):ns) t ) = do tf' <- desugarType tf    
                                           tv' <- desugarType tv                                   
                                           if null ns then do t' <- desugar' t
                                                              return $ Fix i f tf' v tv' t'
                                           else do let slam = Slam i ns t 
                                                   slam' <- desugar' slam 
                                                   return $ Fix i f tf' v tv' slam'

desugar' (SIfZ i c tt tf) = do c' <- desugar' c
                               tt' <- desugar' tt
                               tf' <- desugar' tf
                               return $ IfZ i c' tt' tf'

desugar' q@(SLet i rec n ls tr stmDef stmBody) = do
       body <- desugar' stmBody
       tr' <- desugarType tr
       if null ls then do def <- desugar' stmDef
                          return $ Let i n  tr' def body
       else do fulltype <- desugarTypeList $ (snd $ unzip ls) ++ [tr]
               if not rec then do let slam = Slam i ls stmDef
                                  lam <- desugar' slam
                                  return $ Let i n fulltype lam body
               else do let (a, ta) = head ls
                       taNoSugar <- desugarType ta 
                       let sfix = SFix i n fulltype ls stmDef   
                       fix <- desugar' sfix                       
                       return $ Let i n fulltype fix body
                       

desugar'':: SConst->Const
desugar'' (SCNat n) = CNat n