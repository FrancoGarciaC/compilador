module Optimizations (optDeclaration) where

import Subst ( substN, subst, closeN)
import Lang
import MonadFD4
import Eval (semOp)
import qualified Data.Map as Map
import Data.ByteString hiding(map, unzip, null)
import Data.List 
import Encoding
import Common 


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
                 {- (t1,change1) <- constantPropagation t 
                 printFD4 $ show t1
                 (t2,change2) <- constantFolding t1 -}     
                 (t3,change) <-  commonSubexpression t                          
                 -- if change1 || change2 then optimizer t2
                 printFD4 $ show t3
                 if change then optimizer t3
                 else return t3

optDeclaration :: MonadFD4 m => Bool -> Decl Term -> m (Decl Term)
optDeclaration opt (Decl p x t) = do     
        t'  <- if opt then optimizer t
               else return t 
        return $ Decl p x t' 


{- Contiene:
    - Termino
    - Cantidad de ocurrencias del termino
    - Profundidad del nodo

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
-}
type Depth = Int
type SubExpMap = Map.Map ByteString (Term,Int,Depth)
type TermMap = Map.Map ByteString Term

commonSubexpression :: MonadFD4 m => Term -> m (Term,Bool)
commonSubexpression t = do tm <- findcommonSubexp t Map.empty 0
                           let tm1 = Map.filter (\(_,n,_) -> n > 1) tm
                           (t',tm2) <- factorizer t tm1                            
                           printFD4 "aaa"
                           let ls = Map.toList tm2    
                           printFD4 $ show ls
                           let ls' = sortBy (\(k1,_) -> \(k2,_) -> let (_,_,d1) = case  Map.lookup k1 tm1 of 
                                                                                    Just x -> x 
                                                                                    _ -> error $ show k1
                                                                       (_,_,d2) = case  Map.lookup k2 tm1 of 
                                                                                    Just x -> x 
                                                                                    _ -> error "bbb"
                                                                   in compare d1 d2)  
                                            ls                              
                               ls'' = map (\(k,(t,_,_)) -> (show k,t)) ls'
                               (names,_) = unzip ls''                           
                           return (wrapperLet (closeN names t') ls'', not $ null names)                                        




-- commonSubexpression :: MonadFD4 m => Term -> m (Term,Bool)
-- commonSubexpression t = do m <- commonSubexpression' t Map.empty
--                            let vals = elems m 
--                            commonSubexpression'' t vals

{-
Cuenta cuantas veces una subexpresion se repite y no las cuenta si tienen efectos laterales
-}               
findcommonSubexp:: MonadFD4 m => Term -> SubExpMap -> Depth -> m SubExpMap
findcommonSubexp t@(V i v) tm _ = return tm

findcommonSubexp t@(Const i c) tm _ = return tm

findcommonSubexp (Lam _ _ _ t) tm n = findcommonSubexp t tm (n+1)

findcommonSubexp t@(App i t1 t2) tm n = do       
       tm' <- findcommonSubexp t1 tm (n+1)
       let h1 = hashTerm t1
       let tm1 = if not $ Map.member h1 tm' then tm
                 else tm'
       tm'' <- findcommonSubexp t2 tm1 (n+1)        
       let h2 = hashTerm t2
       let tm2 = if not $ Map.member h2 tm'' then tm1
                 else tm''
       let b = not (Map.member h1 tm2) || not (Map.member h2 tm2)
       if b then return tm2        
       else countSubexp t tm'' n      

findcommonSubexp t@(BinaryOp i bo t1 t2) tm n = do
       tm' <- findcommonSubexp t1 tm (n+1)
       let h1 = hashTerm t1
       if not (Map.member h1 tm') && not (varOrConst t1)  then return tm
       else do tm'' <- findcommonSubexp t2 tm' (n+1)
               let h2 = hashTerm t2
               if not (Map.member h2 tm'') && not (varOrConst t2) then return tm
               else countSubexp t tm'' n                              

findcommonSubexp t@(IfZ i tz tt tf) tm n = do
        tm' <- findcommonSubexp tz tm (n+1)
        let h1 = hashTerm tz
        let tm1 = if not $ Map.member h1 tm' then tm
                  else tm'
        tm'' <- findcommonSubexp tt tm1 (n+1)
        let h2 = hashTerm tt
        let tm2 = if not $ Map.member h2 tm'' then tm1
                  else tm''
        tm''' <- findcommonSubexp tf tm2 (n+1)
        let h3 = hashTerm tf
        let tm3 = if not $ Map.member h3 tm''' then tm2
                  else tm'''
        let b = not (Map.member h1 tm3) || not (Map.member h2 tm3) || not (Map.member h3 tm3)
        if b then return tm3
        else countSubexp t tm3 n

findcommonSubexp (Print _ _ t) tm n = findcommonSubexp t tm (n+1)

findcommonSubexp (Fix _ _ _ _ _ t) tm n = findcommonSubexp t tm (n+1)

countSubexp :: MonadFD4 m => Term -> SubExpMap -> Depth -> m SubExpMap
countSubexp t tm depth = return $  let h = hashTerm t in
                                   case Map.lookup h tm of 
                                       Nothing -> Map.insert h (t,1,depth) tm
                                       Just (_,n,_) -> Map.insert h (t,n+1,depth) tm


{- 
Reemplazar expresiones que suceden varias veces por variables libres
-}

factorizer :: MonadFD4 m => Term -> SubExpMap -> m (Term,SubExpMap)

factorizer t@(V _ _) tm =  return (t,tm) 

factorizer t@(Const _ _) tm = return (t,tm)

factorizer  t@(Lam i n ty t1) tm = do       
       (t1',tm') <- factorizer t1 tm
       return (Lam i n ty t1',tm')       

factorizer t@(App i t1 t2) tm = do let h = hashTerm t
                                   (t1',tm') <- factorizer t1 tm
                                   (t2',tm'') <- factorizer t2 tm'
                                   let app = App i t1' t2'
                                   case Map.lookup h tm of 
                                          Nothing -> return (app,semInsert h app tm'')
                                          _ -> return (V i (Free $ show h),tm'')


factorizer t@(Print i s t1) tm = do let h = hashTerm t
                                    (t1',tm') <- factorizer t1 tm
                                    let print = Print i s t1'
                                    return (print,tm')

factorizer t@(BinaryOp i bo t1 t2) tm = do 
       let h = hashTerm t
       (t1',tm') <- factorizer t1 tm
       (t2',tm'') <- factorizer t2 tm'       
       let bot = BinaryOp i bo t1' t2'
       printFD4 $ show bot
       case Map.lookup h tm of 
              Nothing -> return (bot,tm'')
              _ -> return (V i (Free $ show h),semInsert h bot tm'')
              
factorizer t@(IfZ i tz tt tf) tm = do let h = hashTerm t
                                      (tz',tm') <- factorizer tz tm
                                      (tt',tm'') <- factorizer tt tm'
                                      (tf',tm''') <- factorizer tf tm''
                                      let ifz = IfZ i tz' tt' tf'
                                      case Map.lookup h tm of
                                              Nothing -> return (ifz,semInsert h ifz tm''')
                                              _ -> return (V i (Free $ show h),tm''')
                                   
factorizer  t@(Let i n ty t1 t2) tm = do 
       let h = hashTerm t 
       (t1',tm') <- factorizer t1 tm
       (t2',tm'') <- factorizer t1 tm'
       let t' = Let i n ty t1' t2'
       case Map.lookup h tm of 
              Nothing -> return (t',tm')
              _ -> return (V i (Free $ show h), semInsert h t' tm'')                                   

factorizer t@(Fix i n1 ty1 n2 ty2 t1) tm = do
       (t1',tm') <- factorizer t1 tm
       return (Fix i n1 ty1 n2 ty2 t1',tm')       


{-
Envolver un término con lets 
  | Let info Name Ty (Tm info var)  (Tm info var)

-}
wrapperLet ::  Term -> [(String,Term)] -> Term

wrapperLet t [] = t 

wrapperLet t ((v,trm):xs) = let t' = Let (Pos 0 0) v NatTy trm t
                            in wrapperLet t' xs



varOrConst :: Term -> Bool
varOrConst (V _ _) = True
varOrConst (Const _ _) = True
varOrConst _ = False



semInsert ::  ByteString -> Term -> SubExpMap -> SubExpMap
semInsert h t m = let (_,n,d) = case Map.lookup h m of 
                                   Just x -> x
                                   _ -> error $ show t
                    
                  in Map.insert h (t,n,d) m