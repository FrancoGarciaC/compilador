module IR where

import Lang

data Ir info  = IrVar Ty Name
        | IrGlobal Ty Name
        | IrCall Ty Ir [Ir]
        | IrConst Ty Const
        | IrPrint Ty  String Ir
        | IrBinaryOp Ty BinaryOp Ir Ir 
        | IrLet Ty Name Ir Ir
        | IrIfZ Ty Ir Ir Ir
        | MkClosure Name [Ir]
        | IrAccess Ir Int
  deriving Show

data IrDecl =
    IrFun { irDeclName :: Name
          , irDeclArgNames :: [Name]
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