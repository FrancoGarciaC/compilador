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
desugar decl =
               let  pos = sdeclPos decl
                    name = sdeclName decl
                    body = sdeclBody decl
                    args = sdeclArgs decl
                    info = sgetInfo body
                    typ  = sdeclType decl
                    rec = sdeclRec decl
               in  case args of
                    [] -> do body' <- desugar' body
                             return $ Decl pos name body'
                    _ -> if not rec then do body' <- desugar' $ Slam info args body
                                            return $ Decl pos name body'
                         
                         else  case args of
                                  [(x,tx)] -> do body' <- desugar' $ SFix info name (FunTy tx typ) [(x,tx)] body
                                                 return $ Decl pos name body'

                                  _ -> do let  tailArgs = tail args
                                               slam = Slam info tailArgs body
                                          typeNoSugar <- desugarTypeList (snd $ unzip tailArgs) ++ [typ]
                                          let decl' = SDecl pos rec name [head args] typeNoSugar slam
                                          desugar decl'


desugar' :: MonadFD4 m => STerm -> m NTerm
desugar' (Sv i v) = return $ V i v

desugar' (SConst i c) = return $ Const i $ desugar'' c

desugar' (SPrint i str stm) =  do stm' <- desugar' stm
                                  return $ Print i str stm'

desugar' (Slam i m@[(n, t)] stm) = do typeNoSugar <- desugarTypeList m  
                                      stm' <- desugar' stm
                                      return $ Lam i n typeNoSugar stm'

desugar' (Slam i ((n,t):ls) stm) = do stm' <- desugar' $ Slam i ls stm
                                      return $ Lam i n t stm'

desugar' (SBinaryOp i op stm1 stm2) = do stm1' <- desugar' stm1
                                         stm2' <- desugar' stm2
                                         return $ BinaryOp i op stm1' stm2'

                                   
desugar' (SApp i stm1 stm2) = do stm1' <- desugar' stm1 
                                 stm2' <- desugar' stm2
                                 return $ App i stm1' stm2'

desugar' (SFix i f tr [(n, sty)] t) = do t' <- desugar' t
                                         ty <- desugarType sty
                                         return $ Fix i f tr n ty t'

desugar' (SFix i f tr ((n,ty):ns) t ) = do let fun = Slam i ns t
                                           error $ "Era este caso " ++ show tr

desugar' (SIfZ i c tt tf) = do c' <- desugar' c
                               tt' <- desugar' tt
                               tf' <- desugar' tf
                               return $ IfZ i c' tt' tf'

desugar' q@(SLet i rec n ls t stmDef stmBody) = do
       def <- desugar' stmDef
       body <- desugar' stmBody
       typeNoSugar <- desugarTypeList $ (snd $ unzip ls) ++ [t]
       case ls of
            [] -> return $ Let i n  typeNoSugar def body
            _ -> if not rec then do let slam = Slam i ls stmDef
                                        types = snd $ unzip ls
                                    slam' <- desugar' slam
                                    return $ Let i n typeNoSugar slam' body
                 else case ls of
                         [(a,ta)] -> do taNoSugar <- desugarType ta 
                                        let fix = Fix i n t a taNoSugar def
                                        return $ Let i n typeNoSugar fix body
                         ((a,ta):ts)-> do let fun = Slam i ls stmDef
                                          desugar' $ SLet i rec n [(a,ta)] typeNoSugar fun stmBody

desugar'':: SConst->Const
desugar'' (SCNat n) = CNat n