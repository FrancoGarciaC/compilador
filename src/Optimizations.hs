module Optimizations (optDeclaration) where

import Subst ( substN, subst )
import Lang
import MonadFD4
import Eval (semOp)

constantFolding:: MonadFD4 m => Term -> m (Term,Bool)

constantFolding t@(V i v) = return $ (t,False)

constantFolding t@(Const i c) = return $ (t,False)

constantFolding tt@(Lam i n ty t) = do (t',change) <- constantFolding t
                                       return (Lam i n ty t',change)

constantFolding tt@(Print i s t) =  do (t',change) <- constantFolding t
                                       return (Print i s t',change)   

constantFolding (App i t1 t2) = do (t1',change1) <- constantFolding t1                                         
                                   (t2',change2) <- constantFolding t2
                                   return $ (App i t1' t2',change1 || change2) 

constantFolding (BinaryOp i op t1 t2) = do (t1',change1) <- constantFolding t1                                                                 
                                           (t2',change2) <- constantFolding t2
                                           case (t1',t2') of 
                                                 (Const _ (CNat n), Const _ (CNat m)) -> do printFD4 "1" 
                                                                                            return $ (Const i (CNat (semOp op n m)),True)
        
                                                 
                                                 (V _  x, Const _ (CNat n)) -> do printFD4 "2" 
                                                                                  return $ if n == 0 then (V i x,True) 
                                                                                           else (BinaryOp i op t1' t2',change1 || change2)                                             

                                                 (Const _ (CNat n),V _  x) ->  do printFD4 "3" 
                                                                                  return $ if n == 0 then (V i x,True) 
                                                                                           else (BinaryOp i op t1' t2',change1 || change2)   
                                                
                                                 otherwise -> error "Esto no debería pasar"
                                                
                                                                                               

constantFolding (Fix i n1 t1 n2 t2 t) = do (t',change) <- constantFolding t
                                           return (Fix i n1 t1 n2 t2 t',change)                                                                 
                                             
constantFolding (IfZ i c t1 t2) = do 
                                     printFD4 "aca entra"
                                     (c',change1) <- constantFolding c
                                     printFD4 $ show c'
                                     case c' of 
                                            (Const _ (CNat n)) -> do printFD4 "4"
                                                                     if n == 0 then do (t1',change2) <- constantFolding t1
                                                                                       return (t1',True)
                                                                     else do (t2',change3) <- constantFolding t2
                                                                             return (t2',True)
                                            otherwise -> do (t1',change2) <- constantFolding t1
                                                            (t2',change3) <- constantFolding t2
                                                            return $ (IfZ i c' t1' t2',change1 || change2 || change3)
                                        


constantFolding (Let i n typ t1 t2) = do (t1',change1) <- constantFolding t1
                                         (t2',change2) <- constantFolding t2
                                         return $ (Let i n typ t1' t2',change1 || change2)


constantPropagation:: MonadFD4 m => Term -> m (Term,Bool)
constantPropagation t@(V i v) = return $ (t,False)

constantPropagation t@(Const i c) = return $ (t,False)

constantPropagation tt@(Lam i n ty t) = do (t',change) <- constantPropagation t
                                           return (Lam i n ty t',change)
                                        
constantPropagation tt@(App i t1 t2) = do (t1', change1) <- constantPropagation t1
                                          (t2', change2) <- constantPropagation t2
                                          return (App i t1' t2', change1 || change2)

constantPropagation tt@(Print i s t) = do (t',change) <- constantPropagation t
                                          return (Print i s t',change)  

constantPropagation tt@(BinaryOp i op t1 t2) = do (t1',change1) <- constantPropagation t1
                                                  (t2',change2) <- constantPropagation t2
                                                  return (BinaryOp i op t1' t2', change1 || change2)

constantPropagation (Fix i n1 t1 n2 t2 t) = do (t',change) <- constantPropagation t
                                               return (Fix i n1 t1 n2 t2 t',change)
                                               
constantPropagation (IfZ i c t1 t2) = do (c',change1) <- constantPropagation c
                                         (t1', change2) <- constantPropagation t1
                                         (t2', change3) <- constantPropagation t2
                                         return (IfZ i c' t1' t2', change1 || change2 || change3)

constantPropagation tt@(Let i n ty t1 t2) = do 
                                               (t1', change1) <- constantPropagation t1
                                               case t1' of
                                                   (Const _ (CNat n')) -> do printFD4 $ show t1'
                                                                             (t2',change2) <- constantPropagation $ subst t1' t2
                                                                             return  (t2', True)
                                                   
                                                   _ -> return (Let i n ty t1' t2, change1)

optimizer::MonadFD4 m => Term -> m Term
optimizer t = do printFD4 $ "Optimizando"
                 (t1,change1) <- constantPropagation t 
                 printFD4 $ show t1
                 (t2,change2) <- constantFolding t1                                  
                 if change1 || change2 then optimizer t2
                 else return t2

optDeclaration :: MonadFD4 m => Bool -> Decl Term -> m (Decl Term)
optDeclaration opt (Decl p x t) = do     
        printFD4 $ "term:" ++ show t
        t'  <- if opt then optimizer t
               else return t 
        return $ Decl p x t'       
