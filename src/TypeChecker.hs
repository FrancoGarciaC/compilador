{-|
Module      : Typechecker
Description : Chequeo de tipos de términos y declaraciones.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

-}
module TypeChecker (
   tc,
   tcDecl
   ) where

import Lang
import Global
import MonadFD4
import PPrint
import Subst


-- | 'tc' chequea y devuelve el tipo de un término 
-- Si el término no está bien tipado, lanza un error
-- usando la interfaz de las mónadas @MonadFD4@.
tc :: MonadFD4 m => Term         -- ^ término a chequear
                 -> [(Name,Ty)]  -- ^ entorno de tipado
                 -> m TTerm         -- ^ tipo del término
tc (V p (Bound _)) _ = failPosFD4 p "typecheck: No deberia haber variables Bound"
tc (V p t@(Free n)) bs = case lookup n bs of
                           Nothing -> failPosFD4 p $ "Variable no declarada "++ppName n
                           Just ty -> return $ V ty t 

tc (V p t@(Global n)) bs = case lookup n bs of
                            Nothing -> failPosFD4 p $ "Variable no declarada "++ppName n
                            Just ty -> return $ V ty t
tc (Const _ t@(CNat n)) _ = return $ Const NatTy t

tc (Print p str t) bs = do 
      t' <- tc t bs
      let ty = getInfo t'
      expect NatTy ty t
      return $ Print ty str t'

tc (IfZ p c t1 t2) bs = do
       c' <- tc c bs
       let tyc = getInfo c'
       expect NatTy tyc c
       t1' <- tc t1 bs
       t2' <- tc t2 bs
       let ty1 = getInfo t1'
           ty2 = getInfo t2'
       expect ty1 ty2 t2
       return $ IfZ ty1 c' t1' t2'

tc (Lam p v ty t) bs = do         
         t' <- tc (open v t) ((v,ty):bs)
         let tyl = getInfo t'
         return $ Lam (FunTy ty tyl) v ty (close v t') 

tc (App p t u) bs = do
         t' <- tc t bs
         let tyt = getInfo t'
         (dom,cod) <- domCod t tyt         
         u' <- tc u bs
         let uty = getInfo u'      
         expect dom uty u
         return $ App cod t' u' 

         
tc (Fix p f fty x xty t) bs = do      
         (dom, cod) <- domCod (V p (Free f)) fty         
         when (dom /= xty) $ do
           failPosFD4 p "El tipo del argumento de un fixpoint debe coincidir con el \
                        \dominio del tipo de la función"
         let t' = openN [f, x] t
         t'' <- tc t' ((x,xty):(f,fty):bs)
         let ty'' = getInfo t''         
         expect cod ty'' t'         
         return $ Fix fty f fty x xty (closeN [f,x] t'')

tc (Let p v ty def t) bs = do         
         def' <- tc def bs
         let ty' = getInfo def'  
         expect ty ty' def
         t' <- tc (open v t) ((v,ty):bs)
         let letTy = getInfo t'
         return $ Let letTy v ty def' (close v t')


tc (BinaryOp p op t u) bs = do
         t' <- tc t bs
         let tty = getInfo t'         
         expect NatTy tty t
         u' <- tc u bs
         let uty = getInfo u'
         expect NatTy uty u
         return $ BinaryOp NatTy op t' u'


         

-- | @'typeError' t s@ lanza un error de tipo para el término @t@ 
typeError :: MonadFD4 m => Term   -- ^ término que se está chequeando  
                        -> String -- ^ mensaje de error
                        -> m a
typeError t s = do 
   ppt <- pp t   
   failPosFD4 (getInfo t) $ "Error de tipo en "++ppt++"\n"++s
 
-- | 'expect' chequea que el tipo esperado sea igual al que se obtuvo
-- y lanza un error si no lo es.
expect :: MonadFD4 m => Ty    -- ^ tipo esperado
                     -> Ty    -- ^ tipo que se obtuvo
                     -> Term  -- ^ término que se está chequeando
                     -> m Ty
expect ty ty' t = if ty == ty' then return ty 
                               else typeError t $ 
              "Tipo esperado: "++ ppTy ty
            ++"\npero se obtuvo: "++ ppTy ty'

-- | 'domCod chequea que un tipo sea función
-- | devuelve un par con el tipo del dominio y el codominio de la función
domCod :: MonadFD4 m => Term -> Ty -> m (Ty, Ty)
domCod t (FunTy d c) = return (d, c)
domCod t ty = typeError t $ "Se esperaba un tipo función, pero se obtuvo: " ++ ppTy ty

-- | 'tcDecl' chequea el tipo de una declaración
-- y la agrega al entorno de tipado de declaraciones globales
tcDecl :: MonadFD4 m  => Ty -> Decl Term -> m TTerm
tcDecl tyDecl (Decl p n t) = do
    --chequear si el nombre ya está declarado
    mty <- lookupTy n    
    s <- get        
    case mty of
         Nothing -> do  t' <- tc t (tyEnv s)   
                        let ty = getInfo t' :: Ty                                           
                        expect tyDecl ty t
                        addTy n ty
                        return t'
         Just _  -> failPosFD4 p $ n ++" ya está declarado"

    