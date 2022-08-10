module ClosureConvert where

import IR
import Lang
import Control.Monad.Trans.State.Lazy
import Control.Monad.Writer.Lazy
import Subst(open,openN)


type ClosureState a = StateT Int (Writer [IrDecl]) a

closureConvert :: TTerm -> String -> [(Name,Ty)] -> [Name]  -> ClosureState Ir

closureConvert (V ty v) f xs fwa = 
      case v of 
            (Global s) -> return $ IrGlobal ty s
            (Free s) -> return $ IrVar ty s
            t -> errorCase t 

closureConvert (BinaryOp ty op t1 t2) f xs fwa = do 
      ir1 <- closureConvert t1 f xs fwa
      ir2 <- closureConvert t2 f xs fwa
      return $  IrBinaryOp op ir1 ir2 

closureConvert (IfZ ty tz tt tf) f xs fwa = do 
      ir1 <- closureConvert tz f xs fwa
      ir2 <- closureConvert tt f xs fwa
      ir3 <- closureConvert tf f xs fwa
      return $ IrIfZ ty ir1 ir2 ir3 

closureConvert (Let ty2 x ty1 t1  t2) f xs fwa = do
      let tt = open x t2      
      ir1 <- closureConvert t1 f xs fwa     
      ir2 <- closureConvert tt f ((x,ty1):xs) fwa
      return $ IrLet ty1 x ir1 ir2
      
closureConvert t@(Lam (FunTy _ tyl) x ty t1) f xs fwa = do
      let tt = open x t1
      level <- get 
      name <- freshen f 
      irt <- closureConvert tt f ((x,ty):xs) fwa
      let decl = IrFun name ([("clo",ClosureTy),(x, ty)]) tyl (declareFreeVars irt "clo" $ reverse xs)
      tell [decl]
      return $ MkClosure name [IrVar ty x | (x,ty) <- xs]

closureConvert (App _ t1 t2) f xs fwa = do
      ir2 <- closureConvert t2 f xs fwa
      ir1 <- closureConvert t1 f xs fwa
      let ir2' = case ir2 of                                              
                   IrGlobal _ n -> MkClosure n []
                   _ -> ir2
      case ir1 of             
            q@(MkClosure n xss)  -> do cloname <- freshen "clo"
                                       return $ IrLet ClosureTy cloname q  (IrCall (IrAccess (IrVar ClosureTy cloname) 0) [IrVar ClosureTy cloname,ir2'] )
            
            IrGlobal ty n -> do var <- freshen "var"
                                return $ if not $ n `elem` fwa then IrCall (IrVar ty n) [MkClosure n [],ir2']   
                                         else IrLet ClosureTy var (IrCall (IrVar ClosureTy n) [MkClosure n [],IrConst (CNat 0)]) (IrCall (IrAccess (IrVar ClosureTy var) 0) [IrVar ClosureTy var,ir2'])
                       
            IrVar ty n ->  return $ IrCall (IrAccess (IrVar ClosureTy n) 0) [IrVar ty n,ir2']                        
            t -> do var <- freshen "var"
                    let tyt = ClosureTy
                    return $ IrLet tyt var t (IrCall (IrAccess (IrVar tyt var) 0) [IrVar tyt var,ir2'])                    


closureConvert (Fix retTy ff tf x tv t) f xs fwa = do 
      let tt = openN [ff,x] t
      irt <- closureConvert tt f ([(ff,tf),(x,tv)] ++ xs) fwa      
      let decl = IrFun ff ([("clo",ClosureTy),(x,tv)]) retTy (declareFreeVars irt "clo" $ reverse xs)
      tell [decl]
      return $ MkClosure ff ((IrVar ClosureTy "clo") : [IrVar t x | (x,t) <- xs])



closureConvert (Print _ s t) f xs fwa = closureConvert t f xs fwa >>= \ir -> return $ IrPrint s ir

closureConvert (Const _ c) f xs fwa = return $ IrConst c

closureConvert t _ _ _ = errorCase t


errorCase t = error $ "No consideramos este caso " ++ show t

-- Dado un ir y el nombre de la variable donde se almacena la clausura
-- declara todas las variables libre en t
declareFreeVars :: Ir -> Name -> [(Name,Ty)] -> Ir
declareFreeVars t _ [] = t
declareFreeVars t clo q@((x,ty):xs) = IrLet ty x (IrAccess (IrVar ClosureTy clo) (length q)) $ declareFreeVars t clo xs

freshen :: Name -> StateT Int (Writer [IrDecl]) Name
freshen n = do i <- get 
               put $ i + 1
               return $ "__" ++ n ++ show i

getClosureName :: Int -> Name 
getClosureName n = "clo" ++ show n


-- declaracion
-- argumentos 
-- una lista de funciones sin argumentos explicitos
fromStateToList :: Decl TTerm -> (Bool,[(Name, Ty)]) -> [Name] -> [IrDecl]
fromStateToList d info fwa = 
  let dName = declName d
      (term, freeVars, ret) = case declBody d of 
                               Lam (FunTy ta ret) var tv t -> (open var t,[(var,tv)], ret)
                               Fix (FunTy ta ret) ff tf var tv t -> (openN [ff,var] t,[(ff,tf),(var,tv)],ret)
                               t -> (t,[],getInfo t)
      irt = closureConvert term dName freeVars fwa
      isVal = fst info
      args = snd info     
      declArg = let args = snd info in if null args then ("dummy", NatTy) else head args               
      ((tf,_),decls) = runWriter $ runStateT irt 0 
  in if isVal then decls ++ [IrVal dName tf]
     else decls ++ [IrFun dName [(dName,ClosureTy),declArg] ret tf]
 

