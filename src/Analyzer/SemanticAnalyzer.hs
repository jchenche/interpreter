-- {-# OPTIONS_GHC -Wall #-}
module Analyzer.SemanticAnalyzer (typeCheckAST, typedProgram, typedPlayground) where

import AST.CommonAST
import qualified AST.PlainAST as PT -- PT stands for plain tree
import AST.TypedAST
-- import Control.Monad.Trans.State -- From the transformers library
import Control.Monad.State (State, runState, put, get) -- From the mtl library
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import qualified Data.Map.Strict as M

data SemanticError = TypeMismatch Type Type
                   | OperandTypeError Type Type
                   | BranchTypeMismatch Type Type
                   | SignatureMismatch Ident
                   | ExprIsNotNum Type
                   | ExprIsNotBool Type
                   | VarNotInScope Ident
                   | FuncNotInScope Ident
                   | VarConflict Ident
                   | FuncConflict Ident
                   | ParamConflict Ident
                   | InputTypeError
    deriving Eq

instance Show SemanticError where
    show (TypeMismatch t1 t2) = "Expecting type " ++ show t1 ++ " but got " ++ show t2
    show InputTypeError = "Can only read integer, float, or string as input"
    show _ = "[TODO] Semantic error, refine later"

type StaticEnv = [M.Map Ident Type]

-- If I have StateT as the top of the monad transformer stack,
-- I only get an error in case of an exception.
-- But if I have ExceptT as the top of the monad transformer stack,
-- I get a pair of error and state in case of an exception.
-- I chose the latter since looking at the environment in case of an exception is a plus
type TypeChecker = ExceptT SemanticError (State StaticEnv)

typeCheckAST :: PT.Prog -> (Either SemanticError Prog, StaticEnv)
typeCheckAST ast = runState (runExceptT (programTypeChecker ast)) []

programTypeChecker :: PT.Prog -> TypeChecker Prog
programTypeChecker (PT.Prog es) =
    do { pushScope
       ; typedEs <- mapM (\e -> typec e) es
       ; return $ Prog typedEs
       }

typec :: PT.Expr -> TypeChecker Expr

typec (PT.Not e) =
    do { typedE <- typec e
       ; if getT typedE /= TBool
         then throwError $ ExprIsNotBool (getT typedE)
         else return $ Not TBool typedE
       }

typec (PT.Neg e) =
    do { typedE <- typec e
       ; let operandType = getT typedE
       ; if operandType /= TInt && operandType /= TFloat
         then throwError $ ExprIsNotNum operandType
         else return $ Neg operandType typedE
       }

typec (PT.Mult e1 e2) = arithTypeChecker e1 e2 Mult

typec (PT.Div e1 e2) = arithTypeChecker e1 e2 Div

typec (PT.Plus e1 e2) = arithTypeChecker e1 e2 Plus

typec (PT.Minus e1 e2) = arithTypeChecker e1 e2 Minus

typec (PT.Lesser e1 e2) = compTypeChecker e1 e2 Lesser

typec (PT.LesserEq e1 e2) = compTypeChecker e1 e2 LesserEq

typec (PT.Greater e1 e2) = compTypeChecker e1 e2 Greater

typec (PT.GreaterEq e1 e2) = compTypeChecker e1 e2 GreaterEq

typec (PT.Equal e1 e2) = compTypeChecker e1 e2 Equal

typec (PT.NotEqual e1 e2) = compTypeChecker e1 e2 NotEqual

typec (PT.And e1 e2) = logicTypeChecker e1 e2 And

typec (PT.Or e1 e2) = logicTypeChecker e1 e2 Or

typec (PT.Func returnType ident params body) =
    do { env <- get
       ; case inTopScope ident env of
             Just _  -> throwError $ FuncConflict ident
             Nothing -> do { let funcSig = makeFuncSig returnType params
                           ; storeIdentInTopScope ident funcSig env
                           ; pushScope
                           ; extendEnvWithParams params
                           ; typedBody <- typec body
                           ; popScope
                           ; if returnType /= getT typedBody
                             then throwError $ TypeMismatch returnType (getT typedBody)
                             else return $ Func funcSig returnType ident params typedBody
                           }
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

typec (PT.Lit v) =
    case v of
        PT.VInt x    -> return $ Lit TInt (VInt x)
        PT.VFloat x  -> return $ Lit TFloat (VFloat x)
        PT.VChar x   -> return $ Lit TChar (VChar x)
        PT.VBool x   -> return $ Lit TBool (VBool x)
        PT.VInts x   -> return $ Lit TInts (VInts x)
        PT.VFloats x -> return $ Lit TFloats (VFloats x)
        PT.VChars x  -> return $ Lit TChars (VChars x)
        PT.VBools x  -> return $ Lit TBools (VBools x)
        PT.VNull     -> return $ Lit TVoid VNull

typec (PT.Block es) =
    do { pushScope
       ; typedEs <- mapM (\e -> typec e) es
       ; popScope
       ; if null typedEs
         then error "Illegal State: Block expressions must contain at least one expression!"
         else return $ Block (getT $ last typedEs) typedEs
       }

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

typec (PT.Input t) =
    case t of
        TInt   -> return $ Input t
        TFloat -> return $ Input t
        TChars -> return $ Input t
        _      -> throwError InputTypeError

typec (PT.Print args) =
    do { typedArgs <- mapM (\arg -> typec arg) args
       ; return $ Print TVoid typedArgs
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

typec (PT.Assign ident e) =
    do { env <- get
       ; case inScope ident env of
             Nothing        -> throwError $ VarNotInScope ident
             Just (Sig _ _) -> throwError $ VarNotInScope ident -- Sig type implies a function
             Just varType   -> do { typedE <- typec e
                                  ; if varType /= getT typedE
                                    then throwError $ TypeMismatch varType (getT typedE)
                                    else return $ Assign varType ident typedE
                                  }
       }

typec (PT.Var ident) =
    do { env <- get
       ; case inScope ident env of
             Nothing        -> throwError $ VarNotInScope ident
             Just (Sig _ _) -> throwError $ VarNotInScope ident
             Just t         -> return $ Var t ident
       }

arithTypeChecker :: PT.Expr -> PT.Expr -> (Type -> Expr -> Expr -> Expr) -> TypeChecker Expr
arithTypeChecker e1 e2 opConstructor =
    do { typedE1 <- typec e1
       ; typedE2 <- typec e2
       ; let leftType = getT typedE1
       ; let rightType = getT typedE2
       ; let resultType = case (leftType, rightType) of
                              (TInt, TInt)     -> TInt
                              (TFloat, TFloat) -> TFloat
                              (TInt, TFloat)   -> TFloat
                              (TFloat, TInt)   -> TFloat
                              (TChars, TChars) -> TChars
                              _                -> TVoid
       ; if resultType == TVoid
         then throwError $ OperandTypeError leftType rightType
         else return $ opConstructor resultType typedE1 typedE2
       }

compTypeChecker :: PT.Expr -> PT.Expr -> (Type -> Expr -> Expr -> Expr) -> TypeChecker Expr
compTypeChecker e1 e2 opConstructor =
    do { typedE1 <- typec e1
       ; typedE2 <- typec e2
       ; let leftType = getT typedE1
       ; let rightType = getT typedE2
       ; let resultType = case (leftType, rightType) of
                              (TInt, TInt)     -> TBool
                              (TFloat, TFloat) -> TBool
                              (TChar, TChar)   -> TBool
                              (TBool, TBool)   -> TBool
                              (TChars, TChars) -> TBool
                              _                -> TVoid
       ; if resultType /= TBool
         then throwError $ OperandTypeError leftType rightType
         else return $ opConstructor TBool typedE1 typedE2
       }

logicTypeChecker :: PT.Expr -> PT.Expr -> (Type -> Expr -> Expr -> Expr) -> TypeChecker Expr
logicTypeChecker e1 e2 opConstructor =
    do { typedE1 <- typec e1
       ; typedE2 <- typec e2
       ; if getT typedE1 /= TBool || getT typedE2 /= TBool
         then throwError $ OperandTypeError (getT typedE1) (getT typedE2)
         else return $ opConstructor TBool typedE1 typedE2
       }

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

-- Store identifier with a type in the top scope of environment
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

-- Store parameter identifiers with their types in the top scope of environment
extendEnvWithParams :: [Param] -> TypeChecker ()
extendEnvWithParams [] = return ()
extendEnvWithParams ((Param t ident):params) =
    do { env <- get
       ; case inTopScope ident env of
             Just _  -> throwError $ ParamConflict ident
             Nothing -> do { storeIdentInTopScope ident t env
                           ; extendEnvWithParams params
                           }
       }

getT :: Expr -> Type
getT (Not t _) = t
getT (Neg t _) = t
getT (Mult t _ _) = t
getT (Div t _ _) = t
getT (Plus t _ _) = t
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
getT (Input t) = t
getT (Print t _) = t
getT (Call t _ _) = t
getT (Assign t _ _) = t
getT (Var t _) = t

typedProgram :: Prog
typedProgram =
    Prog [Define TInt "base" (Lit TInt (VInt 1))
        , Func (Sig TInt [TInt]) TInt "factorial" [Param TInt "num"] 
            (Cond TInt (Equal TBool (Var TInt "num") (Var TInt "base"))
            (Var TInt "base")
            (Mult TInt (Var TInt "num") (Call TInt "factorial" [Minus TInt (Var TInt "num") (Lit TInt (VInt 1))])))
        , Define TFloat "aFloat" (Minus TFloat (Mult TInt (Plus TInt (Neg TInt (Lit TInt (VInt 1))) (Lit TInt (VInt 0))) (Lit TInt (VInt 3))) (Mult TFloat (Mult TFloat (Lit TInt (VInt 4)) (Lit TFloat (VFloat 5.1))) (Lit TInt (VInt 6))))
        , Define TInt "result" (Lit TInt (VInt 10000000))
        , Assign TInt "result" (Call TInt "factorial" [Lit TInt (VInt 5)])
        , Define TChars "name" (Input TChars)
        , Print TVoid [Lit TChars (VChars "Hi, "), Var TChars "name"]
        ]

typedPlayground :: Prog
typedPlayground =
    Prog [Define TInt "x" (Lit TInt (VInt 3))
        , Func (Sig TInt [TInt]) TInt "identFunc" [Param TInt "num"] (Block TInt [Var TInt "x", Mult TInt (Plus TInt (Lit TInt (VInt 1)) (Neg TInt (Var TInt "num"))) (Var TInt "num")])
        , Define TBools "conds" (Lit TBools (VBools [True,True,False,True]))
        , Loop TVoid (Lit TBool (VBool True)) (Cond TInt (Lit TBool (VBool True)) (Plus TInt (Lit TInt (VInt 1)) (Mult TInt (Neg TInt (Cond TInt (Lit TBool (VBool True)) (Var TInt "x") (Var TInt "x"))) (Var TInt "x"))) (Lit TInt (VInt 3)))
        , Define TChars "greet" (Lit TChars (VChars "hello world"))
        , Lit TVoid VNull
        ]
