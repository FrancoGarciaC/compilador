module ClosureConvert where

import IR
import Lang
import Control.Monad.Trans.State.Lazy
import Control.Monad.Writer.Lazy



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


closureConvert :: Term -> StateT Int (Writer [IrDecl]) Ir

 
closureConvert (BinaryOp _ op t1 t2) = do 
      ir1 <- closureConvert t1 
      ir2 <- closureConvert t2
      return $  IrBinaryOp op ir1 ir2 

closureConvert (BinaryOp _ op t1 t2) = do 
      ir1 <- closureConvert t1 
      ir2 <- closureConvert t2
      return $  IrBinaryOp op ir1 ir2 

closureConvert (BinaryOp _ op t1 t2) = do 
      ir1 <- closureConvert t1 
      ir2 <- closureConvert t2
      return $  IrBinaryOp op ir1 ir2

closureConvert (IfZ _ tz tt tf) = do 
      ir1 <- closureConvert tz
      ir2 <- closureConvert tt
      ir3 <- closureConvert tf
      return $ IrIfZ ir1 ir2 ir3 

closureConvert (Let _ n _ t1  t2) = do
      ir1 <- closureConvert t1
      let xs = countArgs t1 [] 
      let decl = if isEmpty xs then IrVal n ir1
                 else IrFun n xs ir1
      put decl
      closureConvert t2



-- considerar si se se puede declarar un fix en una fun anidada
countArgs :: Term -> [String] -> [String]
countArgs (Lam _ n _ t) xs =  countArgs t (n:xs)
countArgs _ xs = xs


closureConvert (Lam _ n _ t2) = do 


let f =  
       let g  in ..
in g a



closureConvert (App _ (V _ v) t) = do 



