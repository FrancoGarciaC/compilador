module CEK(search,fromValtoTerm) where

import Lang
import MonadFD4
import Common


-- Tipo de datos para maquina CEK  
--Valores
data Val = N Int | Clos Closure deriving Show
type Env = [Val]


data Closure = ClosFun Env Name Ty Term | ClosFix Env Name Ty Name Ty Term  deriving Show

data Frame   = FrAp  Env Term 
           | FrClos  Closure
           | FrOpTer  Env BinaryOp Term
           | FrOpVal  BinaryOp Val
           | FrPrint  String
           | FrIf  Env Term Term
           | FrLet Env Term
           deriving Show

-- Continuaciones
type Kont = [Frame]



search :: MonadFD4 m => Term -> Env -> Kont -> m Val
search (Print _ s t) e k = search t e (FrPrint  s:k)
search q@(BinaryOp _  bo t1 t2) e k = search t1 e (FrOpTer e bo t2:k)
search (IfZ _  c t1 t2) e k = search c e (FrIf e t1 t2:k)
search (App _ t1 t2) e k = search t1 e (FrAp  e t2:k)
search (V _ (Global n)) e k = do   
                                res <- lookupDecl n
                                case res of 
                                    Nothing -> failFD4 $ "Variable "++n++" indefinida"
                                    Just t -> search t e k
search (Let i n ty t1 t2) e k = search t1 e ((FrLet e t2):k)

                                                    
search (V _ (Bound n)) e k = destroy (e!!n) k
search q@(Const _ (CNat c)) e k = destroy (N c) k
search (Lam _  n ty t) e k = destroy (Clos (ClosFun e n ty t))  k
search (Fix _  f tf x tv t) e k = destroy (Clos (ClosFix e f tf x tv t)) k     
search t k v = error $ "Esto no lo consideramos "++ show t ++ "," ++ show k++ "," ++ show v


destroy :: MonadFD4 m => Val -> Kont -> m Val
destroy v [] = return v

destroy v@(N c) ((FrPrint  s):k) = do   printFD4 $ s++show c
                                        destroy v k

destroy v ((FrOpTer  e bo t2):k)   = search t2 e (FrOpVal  bo v:k)

destroy (N  c1)  ((FrOpVal bo (N  c2)):k) = destroy (N  (semOp bo c2 c1)) k

destroy (N  c)  ((FrIf  e t1 t2):k) | c == 0 = search t1 e k
                                    | otherwise = search t2 e k        

destroy q@(Clos clos) u@((FrAp e t):k) = search t e (FrClos clos:k)

destroy v (FrClos (ClosFun e _ _ t):k) = search t (v:e) k   

destroy v ((FrClos q@(ClosFix e _ _ _ _ t)):k) = search t (v:Clos q:e) k

destroy v ((FrLet e t):k) = search t (v:e) k

destroy q k = error $ "Esto no lo vimos" ++ show q ++ "," ++ show k

-- | Semántica de operadores binarios
semOp :: BinaryOp -> Int -> Int -> Int
semOp Add x y=  x + y
semOp Sub x y = max 0 (x - y)

fromValtoTerm :: Val -> Term
fromValtoTerm (N n) = Const NoPos (CNat n)
fromValtoTerm (Clos q) = fromClostoTerm q
    where   fromClostoTerm (ClosFun _ x ty t) = Lam NoPos x ty t
            fromClostoTerm  (ClosFix _ f tf x tv t) = Fix NoPos f tf x tv t