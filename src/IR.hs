module IR where

import Lang

data Ir = IrVar Ty Name
        | IrGlobal Ty Name
        | IrCall Ty Ir [Ir]
        | IrConst Const
        | IrPrint String Ir
        | IrBinaryOp BinaryOp Ir Ir 
        | IrLet Ty Name Ir Ir
        | IrIfZ Ty Ir Ir Ir
        | MkClosure Ty Name [Ir]
        | IrAccess Ir Int
  deriving Show


getTypeIr :: Ir -> Ty
getTypeIr (IrVar ty _) = ty
getTypeIr (IrGlobal ty _) = ty
getTypeIr (IrCall ty _ _) = ty
getTypeIr (IrConst _) = NatTy
getTypeIr (IrPrint _ _) = NatTy
getTypeIr (IrBinaryOp _ _ _) = NatTy
getTypeIr (IrLet ty _ _ _) = ty
getTypeIr (IrIfZ ty _ _ _) = ty
getTypeIr (MkClosure _ _ _) = ClosureTy
getTypeIr (IrAccess _ _) = error "No tiene tipo"

data IrDecl =
    IrFun { irDeclName :: Name
          , irDeclArgNames :: [(Name,Ty)]
          , irDeclRetType :: Ty
          , irDeclBody :: Ir
    }
  | IrVal { irDeclName :: Name
          , irDeclDef :: Ir
          }
  deriving Show

newtype IrDecls = IrDecls { irDecls :: [IrDecl] }

{-
La siguiente instancia es sólo para debugging
-}
instance Show IrDecls where
  show (IrDecls decls) =
   concatMap (\d -> show d ++ "\n") decls