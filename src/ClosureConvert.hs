module ClosureConvert where

import IR
import Lang
import Control.Monad.Trans.State.Lazy
import Control.Monad.Writer.Lazy
import Subst(open,openN)


type ClosureState a = StateT Int (Writer [IrDecl]) a

closureConvert :: Term -> String -> [Name] -> [Name] -> ClosureState Ir

closureConvert (V _ v) f xs fwa = 
      case v of 
            (Global s) -> return $ IrGlobal s
            (Free s) -> return $ IrVar s
            t -> errorCase t 

closureConvert (BinaryOp _ op t1 t2) f xs fwa = do 
      ir1 <- closureConvert t1 f xs fwa
      ir2 <- closureConvert t2 f xs fwa
      return $  IrBinaryOp op ir1 ir2 

closureConvert (IfZ _ tz tt tf) f xs fwa = do 
      ir1 <- closureConvert tz f xs fwa
      ir2 <- closureConvert tt f xs fwa
      ir3 <- closureConvert tf f xs fwa
      return $ IrIfZ ir1 ir2 ir3 

closureConvert (Let _ x _ t1  t2) f xs fwa = do
      let tt = open x t2      
      ir1 <- closureConvert t1 f xs fwa     
      ir2 <- closureConvert tt f (x:xs) fwa
      return $ IrLet x ir1 ir2
      
closureConvert t@(Lam _ x ty t1) f xs fwa = do
      let tt = open x t1
      level <- get 
      name <- freshen f 
      irt <- closureConvert tt f (x:xs) fwa
      let decl = IrFun name (["clo",x]) (declareFreeVars irt "clo" $ reverse xs)
      tell [decl]
      return $ MkClosure name [IrVar x | x <- xs]

closureConvert (App _ t1 t2) f xs fwa = do
      ir2 <- closureConvert t2 f xs fwa
      ir1 <- closureConvert t1 f xs fwa
      case ir1 of             
            q@(MkClosure n xss)  -> do cloname <- freshen "clo"
                                       return $ IrLet cloname q  (IrCall (IrAccess (IrVar cloname) 0) [IrVar cloname,ir2] )
            
            IrGlobal n -> do var <- freshen "var"
                             return $ if not $ n `elem` fwa then IrCall (IrVar n) [MkClosure n [],ir2]   
                                      else IrLet var (IrCall (IrVar n) [MkClosure n [],IrConst (CNat 0)]) (IrCall (IrAccess (IrVar var) 0) [IrVar var,ir2])
                       
            IrVar n ->  return $ IrCall (IrAccess (IrVar n) 0) [IrVar n,ir2]                        
            t -> do var <- freshen "var"
                    return $ IrLet var t (IrCall (IrAccess (IrVar var) 0) [IrVar var,ir2])


closureConvert (Fix _ ff _ x _ t) f xs fwa = do 
      let tt = openN [ff,x] t
      irt <- closureConvert tt f (x:xs) fwa
      let decl = IrFun ff (["clo",x]) (declareFreeVars irt "clo" $ reverse xs)
      tell [decl]
      return $ MkClosure ff [IrVar x | x <- xs]



closureConvert (Print _ s t) f xs fwa = closureConvert t f xs fwa >>= \ir -> return $ IrPrint s ir

closureConvert (Const _ c) f xs fwa = return $ IrConst c

closureConvert t _ _ _ = errorCase t


errorCase t = error $ "No consideramos este caso " ++ show t

-- Dado un ir y el nombre de la variable donde se almacena la clausura
-- declara todas las variables libre en t
declareFreeVars :: Ir -> Name -> [String] -> Ir
declareFreeVars t _ [] = t
declareFreeVars t clo q@(x:xs) = IrLet x (IrAccess (IrVar clo) (length q)) $ declareFreeVars t clo xs

freshen :: Name -> StateT Int (Writer [IrDecl]) Name
freshen n = do i <- get 
               put $ i + 1
               return $ "__" ++ n ++ show i

getClosureName :: Int -> Name 
getClosureName n = "clo" ++ show n

fromStateToList :: Decl Term -> (Bool,[Name]) -> [Name] -> [IrDecl]
fromStateToList d info fwa = 
  let dName = declName d
      (term, freeVars) = case declBody d of 
                              Lam _ var _ t -> (open var t,[var])
                              t -> (t,[])
      clo = closureConvert term dName freeVars fwa
      isVal = fst info
      args = snd info     
      declArg = let args = snd info in if null args then "dummy" else head args               
      ((tf,_),decls) = runWriter $ runStateT clo 0 
  in if isVal then decls ++ [IrVal dName tf]
     else decls ++ [IrFun dName ["clo",declArg] tf]
 

