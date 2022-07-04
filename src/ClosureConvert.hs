module ClosureConvert where

import IR
import Lang
import Control.Monad.Trans.State.Lazy
import Control.Monad.Writer.Lazy
import Subst(open)



{-
   V info var
  | Const info Const
  | Lam info Name Ty (Tm info var)
  | App info (Tm info var) (Tm info var)
  | Print info String (Tm info var)
  | BinaryOp info BinaryOp (Tm info var) (Tm info var)
  | Fix info Name Ty Name Ty (Tm info var)
  | IfZ info (Tm info var) (Tm info var) (Tm info var)
  | Let info Name Ty (Tm info var)  (Tm info var)
  
  Ir = IrVar Name
        | IrGlobal Name
        | IrCall Ir [Ir]
        | IrConst Const
        | IrPrint String Ir
        | IrBinaryOp BinaryOp Ir Ir 
        | IrLet Name Ir Ir
        | IrIfZ Ir Ir Ir
        | MkClosure Name [Ir]
        | IrAccess Ir Int


suma (x,y){x+y}

suma5 (x){

      let aux() = access 0 
      return $ (suma [5])
      

}
 Let _ n t

 data IrDecl =
    IrFun { irDeclName :: Name
          , irDeclArgNames :: [Name]
          , irDeclBody :: Ir
    }
  | IrVal { irDeclName :: Name
          , irDeclDef :: Ir
          }
  deriving Show

-}

type ClosureState a = StateT Int (Writer [IrDecl]) a

closureConvert :: Term -> String -> [Name] -> ClosureState Ir

closureConvert (V _ v) f xs = 
      case v of 
            (Global s) -> return $ IrGlobal s
            (Free s) -> return $ IrVar s
            t -> errorCase t 

closureConvert (BinaryOp _ op t1 t2) f xs = do 
      ir1 <- closureConvert t1 f xs 
      ir2 <- closureConvert t2 f xs
      return $  IrBinaryOp op ir1 ir2 

closureConvert (IfZ _ tz tt tf) f xs = do 
      ir1 <- closureConvert tz f xs
      ir2 <- closureConvert tt f xs
      ir3 <- closureConvert tf f xs
      return $ IrIfZ ir1 ir2 ir3 

closureConvert (Let _ x _ t1  t2) f xs = do
      let tt = open x t2      
      ir1 <- closureConvert t1 f xs      
      ir2 <- closureConvert tt f (x:xs) 
      return $ IrLet x ir1 ir2
      
closureConvert t@(Lam _ x ty t1) f xs = do
      let tt = open x t1
      level <- get 
      name <- freshen f -- obtiene un nombre fresco      
      irt <- closureConvert tt f (x:xs)          
      if level == 0 then return irt
      else  do let vars = variableCollector t                     
               let cloname = "clo"
               let decl = IrFun name (cloname:vars) (declareFreeVars irt cloname $ reverse xs)
               tell [decl]
               return $ MkClosure name [IrVar x | x <- xs]

closureConvert (App _ t1 t2) f xs = do
      ir2 <- closureConvert t2 f xs
      ir1 <- closureConvert t1 f xs
      case ir1 of 
            IrCall n xss -> return $ IrCall n $ xss ++ [ir2]
            MkClosure n xss  -> return $ IrCall (IrVar n) [MkClosure n xss,ir2]
            IrGlobal n -> return $ IrCall (IrVar n) [ir2]   
            IrVar n ->  return $ IrCall (IrAccess (IrVar n) 0) [IrVar n,ir2]                        
            tt -> errorCase tt


closureConvert (Print _ s t) f xs = closureConvert t f xs >>= \ir -> return $ IrPrint s ir

closureConvert (Const _ c) f xs = return $ IrConst c

closureConvert t _ _ = errorCase t


errorCase t = error $ "No consideramos este caso " ++ show t

variableCollector :: Term -> [Name]
variableCollector (Lam _ x _ t) = x : (variableCollector t)
variableCollector (Fix _ x _ _ _ t) = x : (variableCollector t)
variableCollector _ = []

-- Dado un ir y el nombre de la variable donde se almacena la clausura
-- declara todas las variables libre en t
declareFreeVars :: Ir -> Name -> [String] -> Ir
declareFreeVars t _ [] = t
declareFreeVars t clo q@(x:xs) = IrLet x (IrAccess (IrVar clo) (length q)) $ declareFreeVars t clo xs

freshen :: Name -> StateT Int (Writer [IrDecl]) Name
freshen n = do i <- get 
               put $ i + 1
               return $ "__" ++ n ++ show i