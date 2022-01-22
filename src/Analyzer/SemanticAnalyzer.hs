-- {-# OPTIONS_GHC -Wall #-}
module Analyzer.SemanticAnalyzer where

import AST.CommonAST
import qualified AST.PlainAST as PT
import AST.TypedAST
-- import Control.Monad.Trans.State -- From the transformers library
import Control.Monad.State -- From the mtl library
import Control.Monad.Except
import qualified Data.Map.Strict as M

data SemanticError = TypeMismatch Type Type
                   | SignatureMismatch Ident
                   | ExprIsNotBool Type
                   | BranchTypeMismatch Type Type
                   | VarNotInScope Ident
                   | FuncNotInScope Ident
                   | VarConflict Ident
                   | FuncConflict Ident
                   | ParamConflict Ident
    deriving Eq

instance Show SemanticError where
    show (TypeMismatch t1 t2) = show t1 ++ "doesn't match" ++ show t2
    show _ = "[TODO] Error, refine later"

type StaticEnv = [M.Map Ident Type]

-- If I have StateT as the top of the monad transformer stack,
-- I only get an error in case of an exception.
-- But if I have ExceptT as the top of the monad transformer stack,
-- I get a pair of error and state in case of an exception.
-- I chose the latter since looking at the environment in case of an exception is a plus
type TypeChecker = ExceptT SemanticError (State StaticEnv)

-- temp :: PT.Prog -> TypeChecker Prog
-- temp (PT.Prog es) =
--     do {
--          env <- get
--     --    ; lift $ lift $ print "hi"
--        ; put (M.empty:env)
--        ; throwError (FuncNotInScope "hello")
--        ; return $ Prog [Lit TInt (VInt 3)]
--        }

typeCheckAST :: PT.Prog -> (Either SemanticError Prog, StaticEnv)
typeCheckAST ast = runState (runExceptT (programTypeChecker ast)) []

programTypeChecker :: PT.Prog -> TypeChecker Prog
programTypeChecker (PT.Prog es) =
    do { pushScope
       ; typedEs <- mapM (\e -> typec e) es
       ; return $ Prog typedEs
       }

typec :: PT.Expr -> TypeChecker Expr
typec (PT.Var ident) =
    do { env <- get
       ; case inScope ident env of
             Nothing        -> throwError $ VarNotInScope ident
             Just (Sig _ _) -> throwError $ VarNotInScope ident -- Sig type implies a function
             Just t         -> return $ Var t ident
       }

typec (PT.Define declaredType ident e) =
    do { typedE <- typec e
       ; if declaredType /= getT typedE
         then throwError $ TypeMismatch declaredType (getT typedE)
         else do { env <- get
                 ; case inTopScope ident env of
                       Just _  -> throwError $ VarConflict ident
                       Nothing -> do { storeIdentInTopScope ident declaredType env
                                     ; return $ Define declaredType ident typedE
                                     }
                 }
       }

typec (PT.Func returnType ident params body) =
    do { env <- get
       ; case inTopScope ident env of
             Just _  -> throwError $ FuncConflict ident
             Nothing -> do { storeIdentInTopScope ident (makeFuncSig returnType params) env
                           ; pushScope
                           ; extendEnvWithParamTypes params
                           ; typedBody <- typec body
                           ; if returnType /= getT typedBody
                             then throwError $ TypeMismatch returnType (getT typedBody)
                             else do { popScope
                                     ; return $ Func (makeFuncSig returnType params) returnType ident params typedBody
                                     }
                           }
       }

typec (PT.Call ident args) =
    do { env <- get
       ; case inScope ident env of
             Just (Sig returnType paramTypes) -> do { typedArgs <- mapM (\arg -> typec arg) args
                                                    ; let argTypes = map getT typedArgs
                                                    ; let allTypesMatch = all (\(pt, at) -> pt == at) (zip paramTypes argTypes)
                                                    ; if not (length paramTypes == length argTypes && allTypesMatch)
                                                      then throwError $ SignatureMismatch ident
                                                      else return $ Call returnType ident typedArgs
                                                    }
             _                                -> throwError $ FuncNotInScope ident
       }

typec (PT.Not e) =
    do { typedE <- typec e
       ; return $ Not (getT typedE) typedE
       }

typec (PT.Block es) = 
    do { pushScope
       ; typedEs <- mapM (\e -> typec e) es
       ; popScope
       ; if null typedEs
         then error "Illegal State: Block expressions must contain at least one expression!"
       ; else return $ Block (getT $ last typedEs) typedEs
       }

typec (PT.Lit e) =
    case e of
        PT.VInt v        -> return $ Lit TInt (VInt v)
        PT.VFloat v      -> return $ Lit TFloat (VFloat v)
        PT.VChar v       -> return $ Lit TChar (VChar v)
        PT.VBool v       -> return $ Lit TBool (VBool v)
        PT.VInts v       -> return $ Lit TInts (VInts v)
        PT.VFloats v     -> return $ Lit TFloats (VFloats v)
        PT.VChars v      -> return $ Lit TChars (VChars v)
        PT.VBools v      -> return $ Lit TBools (VBools v)
        PT.VNull         -> return $ Lit TVoid VNull
        PT.Closure _ _ _ -> error "Illegal State: Closure doesn't exist during type-checking!"

typec (PT.Cond e1 e2 e3) =
    do { typedE1 <- typec e1
       ; if getT typedE1 /= TBool
         then throwError $ ExprIsNotBool (getT typedE1)
         else do { typedE2 <- typec e2
                 ; typedE3 <- typec e3
                 ; if getT typedE2 /= getT typedE3
                   then throwError $ BranchTypeMismatch (getT typedE2) (getT typedE3)
                   else return $ Cond (getT typedE2) typedE1 typedE2 typedE3
                 }
       }

typec (PT.Loop e body) =
    do { typedE <- typec e
       ; if getT typedE /= TBool
         then throwError $ ExprIsNotBool (getT typedE)
         else do { typedBody <- typec body
                 ; return $ Loop TVoid typedE typedBody
                 }
       }

typec _ = error "Illegal State: Shouldn't be here!"

-- Extract type of identifier from the environment starting from the inner most/top scope,
-- stopping when you encounter one (this allows variable shadowing),
-- returning Nothing if it's absent
inScope :: Ident -> StaticEnv -> Maybe Type
inScope ident []             = Nothing
inScope ident (scope:scopes) =
    case ident `M.lookup` scope of
        Nothing -> inScope ident scopes
        t       -> t

-- Extract type of identifier from the top scope, return Nothing if it's absent
inTopScope :: Ident -> StaticEnv -> Maybe Type
inTopScope ident [] = error "Illegal State: Looking at an empty environment!"
inTopScope ident (scope:_) = ident `M.lookup` scope

-- Store identifier with a type in the environment
storeIdentInTopScope :: Ident -> Type -> StaticEnv -> TypeChecker ()
storeIdentInTopScope _ _ [] = error "Illegal State: Storing variable to empty environment!"
storeIdentInTopScope ident t (scope:scopes) = put ((M.insert ident t scope):scopes)

-- Push a new scope to the top
pushScope :: TypeChecker ()
pushScope =
    do { env <- get
       ; put (M.empty:env)
       }

-- Pop the scope at the top
popScope :: TypeChecker ()
popScope =
    do { env <- get
       ; case env of
             [] -> error "Illegal State: Popping a scope from an empty environment!"
             (_:scopes) -> put scopes
       }

-- Make a function signature from its return type and parameter types
makeFuncSig :: Type -> [Param] -> Type
makeFuncSig returnType params = Sig returnType $ map (\(Param t _) -> t) params

extendEnvWithParamTypes :: [Param] -> TypeChecker ()
extendEnvWithParamTypes [] = return ()
extendEnvWithParamTypes ((Param t ident):params) =
    do { env <- get
       ; case inTopScope ident env of
             Just _  -> throwError $ ParamConflict ident
             Nothing -> do { storeIdentInTopScope ident t env
                           ; extendEnvWithParamTypes params
                           }
       }

getT :: Expr -> Type
getT (Not t _) = t
getT (Neg t _) = t
getT (Mult t _ _) = t
getT (Div t _ _) = t
getT (Add t _ _) = t
getT (Minus t _ _) = t
getT (Lesser t _ _) = t
getT (LesserEq t _ _) = t
getT (Greater t _ _) = t
getT (GreaterEq t _ _) = t
getT (Equal t _ _) = t
getT (NotEqual t _ _) = t
getT (And t _ _) = t
getT (Or t _ _) = t
getT (Define t _ _) = t
getT (Lit t _) = t
getT (Block t _) = t
getT (Func t _ _ _ _) = t
getT (Cond t _ _ _) = t
getT (Loop t _ _) = t
getT (Call t _ _) = t
getT (Assign t _ _) = t
getT (Var t _) = t

typedSimple =
    (Right (Prog [Define TInt "x" (Lit TInt (VInt 1))
         , Define TFloat "y" (Lit TFloat (VFloat 2.0))
         , Not TInt (Var TInt "x")
         , Var TFloat "y"])
   , [M.fromList [("x",TInt),("y",TFloat)]])

typedProgram :: Prog
typedProgram =
    Prog [Define TInt "base" (Lit TInt (VInt 1))
        , Func (Sig TInt [TInt]) TInt "factorial" [Param TInt "num"] 
            (Cond TInt (Equal TBool (Var TInt "num") (Var TInt "base"))
            (Var TInt "base")
            (Mult TInt (Var TInt "num") (Call TInt "factorial" [Minus TInt (Var TInt "num") (Lit TInt (VInt 1))])))
        , Define TFloat "aFloat" (Minus TFloat (Mult TInt (Add TInt (Neg TInt (Lit TInt (VInt 1))) (Lit TInt (VInt 0))) (Lit TInt (VInt 3))) (Mult TFloat (Mult TFloat (Lit TInt (VInt 4)) (Lit TFloat (VFloat 5.1))) (Lit TInt (VInt 6))))
        , Define TInt "result" (Lit TInt (VInt 10000000))
        , Assign TInt "result" (Call TInt "factorial" [Lit TInt (VInt 5)])
    ]

typedPlayground :: Prog
typedPlayground =
    Prog [Define TInt "x" (Lit TInt (VInt 3))
        , Func (Sig TInt [TInt]) TInt "identFunc" [Param TInt "num"] (Block TInt [Var TInt "x", Mult TInt (Add TInt (Lit TInt (VInt 1)) (Neg TInt (Var TInt "num"))) (Var TInt "num")])
        , Define TBools "conds" (Lit TBools (VBools [True,True,False,True]))
        , Loop TVoid (Lit TBool (VBool True)) (Cond TInt (Lit TBool (VBool True)) (Add TInt (Lit TInt (VInt 1)) (Mult TInt (Neg TInt (Cond TInt (Lit TBool (VBool True)) (Var TInt "x") (Var TInt "x"))) (Var TInt "x"))) (Lit TInt (VInt 3)))
        , Define TChars "greet" (Lit TChars (VChars "hello world"))
        , Lit TVoid VNull
    ]
