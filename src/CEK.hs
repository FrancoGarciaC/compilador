module CEK where

import Lang    


data Val info = N info Const  | ClosFun Env Int Term | ClosFix Env Name Int Term | 

data Frame info  = FrAp info Env Term 
           | FrClos info Closure
           | FrOpTer info Env Op Term
           | FrOpVal info Op Val
           | FrPrint info String
           | FrIf info Env Term Term


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



search :: MonadFD4 m => Term -> Env -> Kont -> m Val

search (Print i s t) e k = search t e ((FrPrint i s):k)
search (BinaryOp i bo t1 t2) e k = search t1 e ((FrOpTer i e bo t2):k)
search (IfZ i c t1 t2) e k = search c e ((FrIf i e t1 t2):k)
search (App i t1 t2) e k = search t1 e ((FrAp i e t):k)
search (V i v) e k = case lookup e v of 
                        Nothing-> failFD4 $ "Variable "++v++" indefinida"
                        Just n -> destroy n k
search (Const i c) e k = destroy (N i c) k
search (Lam i name Ty (Tm info var)) e k = let clos=ClosFun e Int Term



destroy :: MonadFD4 m => Val -> Kont -> m Val
destroy v@(N i c) ((FrPrint i s):k) = do printFD4 (s++show c)
                                         destroy v k
destroy v ((FrOpTer i e bo t2):k)   = search t2 e ((FrOpVal i bo v):k)                                       
destroy (N i c1)  ((FrOpVal i e bo (N i c2)):k) = let v3=N i (semOp bo c1 c2)
                                                  in destroy v3 k
destroy (N i c)  (FrIf info e t1 t2):k) = 
        let  res=if c == CNat 0  then t1
                 else t2
        in search res e k
                                                    





-- | Semántica de operadores binarios
semOp :: BinaryOp -> Int -> Int -> Int
semOp Add x y=  x + y
semOp Sub x y = max 0 (x - y)
