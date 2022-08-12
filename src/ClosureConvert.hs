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
      return $ IrLet ty1 x ir1 ty2 ir2
      
closureConvert t@(Lam typ x ty t1) f xs fwa = do
      let tt = open x t1
      ns <- freshen [f] 
      let name = head ns
          ret = getCod typ
      irt <- closureConvert tt f ((x,ty):xs) fwa
      
      -- en caso de que el argumento sea una funcion creamos una clausura para la misma
      let  irt' = case ty of 
                    
                    FunTy _ _ -> IrLet ClosureTy (x ++ "_clo") (MkClosure ty x []) (getTypeIr irt) irt
                    
                    _ -> irt
             
           decl = IrFun name ([("clo",ClosureTy),(x, ty)]) ret (declareFreeVars f (getTypeIr irt') irt' "clo" $ reverse xs)
     
      tell [decl]
      return $ MkClosure typ name [IrVar ty x | (x,ty) <- xs, x /= f]

closureConvert (App _ t1 t2) f xs fwa = do
      ir2 <- closureConvert t2 f xs fwa
      ir1 <- closureConvert t1 f xs fwa
      let ir2' = case ir2 of                                              
                   IrGlobal ty n -> MkClosure ty n []
                   _ -> ir2
      case ir1 of             
            q@(MkClosure ty n xss)  -> do ns <- freshen ["clo","var"]
                                          let cloname = ns !! 0 
                                              fname = ns !! 1 
                                              cod = getCod ty
                                          return $ IrLet ClosureTy cloname q cod 
                                                   (IrLet ty fname (IrAccess (IrVar ClosureTy cloname) 0) cod (IrCall cod (IrVar ty fname)  [IrVar ClosureTy cloname,ir2']))
            
            v@(IrGlobal ty n) -> do ns <- freshen ["clo","var"]
                                    let cloname = ns !! 0
                                        varname = ns !! 1 
                                        cod = getCod ty
                                    return $ if not $ n `elem` fwa then IrCall cod v [MkClosure ty n [],ir2']   
                                             else IrLet ClosureTy cloname (IrCall cod (IrVar ty n) [MkClosure ty n [],IrConst (CNat 0)]) cod $
                                                  IrLet ty varname (IrAccess (IrVar ClosureTy cloname) 0) cod
                                                  (IrCall cod (IrVar ty varname)  [IrVar ty varname,ir2'])
                       
            v@(IrVar ty n) ->  return $ IrCall (getCod ty) v [IrVar ClosureTy (n ++ "_clo"), ir2'] 

            c@(IrCall ty n args) -> do  ns <- freshen ["clo","var"]
                                        let cloname = ns !! 0 
                                            varname = ns !! 1    
                                            clovar =  IrVar ClosureTy cloname  
                                            retTy = getCod ty                                    
                                        return $ IrLet ClosureTy cloname c retTy $
                                                 IrLet ty varname  (IrAccess clovar  0) retTy 
                                                 (IrCall retTy (IrVar ty varname) [clovar,ir2])
            
                                 
            t -> do let tyT = getTypeIr t
                    ns <- freshen ["clo","var"]
                    let tyt = ClosureTy
                        cloname = ns !! 0
                        varname = ns !! 1 
                        tyRet = getCod tyT
                        clovar = IrVar ClosureTy cloname 
                    return $ IrLet ClosureTy cloname t tyRet 
                             (IrLet tyT varname (IrAccess clovar 0) tyRet
                             (IrCall tyRet (IrVar tyT varname) [clovar,ir2]))


closureConvert (Fix retTy ff tf x tv t) f xs fwa = do 
      let tt = openN [ff,x] t
      irt <- closureConvert tt f ([(ff,tf),(x,tv)] ++ xs) fwa      
      let decl = IrFun ff ([("clo",ClosureTy),(x,tv)]) retTy (declareFreeVars f (getTypeIr irt) irt "clo" $ reverse xs)
      tell [decl]
      return $ MkClosure tf ff ((IrVar ClosureTy "clo") : [IrVar t x | (x,t) <- xs, x /= f])



closureConvert (Print _ s t) f xs fwa = closureConvert t f xs fwa >>= \ir -> return $ IrPrint s ir

closureConvert (Const _ c) f xs fwa = return $ IrConst c

closureConvert t _ _ _ = errorCase t


errorCase t = error $ "No consideramos este caso " ++ show t

-- Dado un ir y el nombre de la variable donde se almacena la clausura
-- declara todas las variables libre en t
declareFreeVars :: Name -> Ty -> Ir -> Name -> [(Name,Ty)] -> Ir
declareFreeVars _ _ t _ [] = t
declareFreeVars f tBody t clo q@((x,ty):xs) =
                                case ty of 
                                       
                                       (FunTy _ _) -> let xClo = x ++ "_clo" in
                                                      -- IrLet ClosureTy xClo (IrAccess (IrVar ClosureTy clo) (length q)) tBody
                                                      (IrLet ty x (IrAccess (IrVar ClosureTy xClo) 0) tBody $ declareFreeVars f tBody t clo xs)
                                                                                             
                                       _ -> IrLet ty x (IrAccess (IrVar ClosureTy clo) (length q)) tBody $ declareFreeVars f tBody t clo xs




freshen :: [Name] -> ClosureState [Name]
freshen ns = do i <- get 
                put $ i + 1
                return $ map (\n -> "__" ++ n ++ show i) ns

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
                               Fix (FunTy ta ret) ff tf var tv t -> (openN [ff,var] t,[(ff,tf),(var,tv),(dName ++ "_clo",ClosureTy)],ret)
                               t -> (t,[],getInfo t)
      irt = closureConvert term dName freeVars fwa
      isVal = fst info
      args = snd info     
      declArg = let args = snd info in if null args then ("dummy", NatTy) else head args               
      ((tf,_),decls) = runWriter $ runStateT irt 0 
  in if isVal then decls ++ [IrVal dName tf]
     else decls ++ [IrFun dName [(dName ++ "_clo",ClosureTy),declArg] ret tf]
 

getCod :: Ty -> Ty 
getCod (FunTy _ c) = c
getCod _ = error "Esta variable no representa una funcion"