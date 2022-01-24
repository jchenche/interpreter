module Interpreter.Interpreter where

import AST.CommonAST
import AST.TypedAST
import Control.Monad.State (StateT, runStateT, put, get)
import Control.Monad.Except (ExceptT, runExceptT, throwError, liftIO)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import qualified Data.Map.Strict as M

data RuntimeError = DivByZero
                  | ArrayOutOfBound
                  | AttemptToPrintFunc
                  | InputError String
    deriving Eq

instance Show RuntimeError where
    show DivByZero = "Attempt to divide by zero"
    show (InputError msg) = msg
    show _ = "[TODO] Runtime error, refine later"

type Interpreter = ExceptT RuntimeError (StateT DynamicEnv IO)

interpretAST :: Prog -> IO (Either RuntimeError (), DynamicEnv)
interpretAST ast = runStateT (runExceptT (programEvaluator ast)) []

programEvaluator :: Prog -> Interpreter ()
programEvaluator (Prog es) =
    do { pushScope
       ; vs <- mapM (\e -> eval e) es
       ; return ()
       }

eval :: Expr -> Interpreter Val

eval (Not _ e) =
    do { v <- eval e
       ; case v of
             VBool True  -> return $ VBool False
             VBool False -> return $ VBool True
             _           -> error "Illegal State: Cannot apply Not to a non-boolean!"
       }

eval (Neg _ e) =
    do { v <- eval e
       ; case v of
             VInt x   -> return $ VInt (-x)
             VFloat x -> return $ VFloat (-x)
             _        -> error "Illegal State: Cannot negate a non-number!"
       }

eval (Mult _ e1 e2) =
    do { v1 <- eval e1
       ; v2 <- eval e2
       ; case (v1, v2) of
             (VInt x1, VInt x2)     -> return $ VInt (x1 * x2)
             (VFloat x1, VFloat x2) -> return $ VFloat (x1 * x2)
             (VInt x1, VFloat x2)   -> return $ VFloat (fromIntegral x1 * x2)
             (VFloat x1, VInt x2)   -> return $ VFloat (x1 * fromIntegral x2)
             _                      -> error "Illegal State: Arithmetic performed on non-numbers!"
       }

eval (Div _ e1 e2) =
    do { v1 <- eval e1
       ; v2 <- eval e2
       ; case (v1, v2) of
             (_, VInt 0)            -> throwError DivByZero
             (_, VFloat 0)          -> throwError DivByZero
             (VInt x1, VInt x2)     -> return $ VFloat (fromIntegral x1 / fromIntegral x2)
             (VFloat x1, VFloat x2) -> return $ VFloat (x1 / x2)
             (VInt x1, VFloat x2)   -> return $ VFloat (fromIntegral x1 / x2)
             (VFloat x1, VInt x2)   -> return $ VFloat (x1 / fromIntegral x2)
             _                      -> error "Illegal State: Arithmetic performed on non-numbers!"
       }

eval (Plus _ e1 e2) =
    do { v1 <- eval e1
       ; v2 <- eval e2
       ; case (v1, v2) of
             (VInt x1, VInt x2)     -> return $ VInt (x1 + x2)
             (VFloat x1, VFloat x2) -> return $ VFloat (x1 + x2)
             (VInt x1, VFloat x2)   -> return $ VFloat (fromIntegral x1 + x2)
             (VFloat x1, VInt x2)   -> return $ VFloat (x1 + fromIntegral x2)
             _                      -> error "Illegal State: Arithmetic performed on non-numbers!"
       }

eval (Minus _ e1 e2) =
    do { v1 <- eval e1
       ; v2 <- eval e2
       ; case (v1, v2) of
             (VInt x1, VInt x2)     -> return $ VInt (x1 - x2)
             (VFloat x1, VFloat x2) -> return $ VFloat (x1 - x2)
             (VInt x1, VFloat x2)   -> return $ VFloat (fromIntegral x1 - x2)
             (VFloat x1, VInt x2)   -> return $ VFloat (x1 - fromIntegral x2)
             _                      -> error "Illegal State: Arithmetic performed on non-numbers!"
       }

eval (Lesser _ e1 e2) =
    do { v1 <- eval e1
       ; v2 <- eval e2
       ; case (v1, v2) of
             (VInt x1, VInt x2)     -> evalEquality x1 x2
             (VFloat x1, VFloat x2) -> evalEquality x1 x2
             (VChar x1, VChar x2)   -> evalEquality x1 x2
             (VBool x1, VBool x2)   -> evalEquality x1 x2
             (VChars x1, VChars x2) -> evalEquality x1 x2
             _                      -> error "Illegal State: Comparison performed on wrong values!"
       }
    where evalEquality x1 x2 = return $ if x1 < x2 then (VBool True) else (VBool False)

eval (LesserEq _ e1 e2) =
    do { v1 <- eval e1
       ; v2 <- eval e2
       ; case (v1, v2) of
             (VInt x1, VInt x2)     -> evalEquality x1 x2
             (VFloat x1, VFloat x2) -> evalEquality x1 x2
             (VChar x1, VChar x2)   -> evalEquality x1 x2
             (VBool x1, VBool x2)   -> evalEquality x1 x2
             (VChars x1, VChars x2) -> evalEquality x1 x2
             _                      -> error "Illegal State: Comparison performed on wrong values!"
       }
    where evalEquality x1 x2 = return $ if x1 <= x2 then (VBool True) else (VBool False)

eval (Greater _ e1 e2) =
    do { v1 <- eval e1
       ; v2 <- eval e2
       ; case (v1, v2) of
             (VInt x1, VInt x2)     -> evalEquality x1 x2
             (VFloat x1, VFloat x2) -> evalEquality x1 x2
             (VChar x1, VChar x2)   -> evalEquality x1 x2
             (VBool x1, VBool x2)   -> evalEquality x1 x2
             (VChars x1, VChars x2) -> evalEquality x1 x2
             _                      -> error "Illegal State: Comparison performed on wrong values!"
       }
    where evalEquality x1 x2 = return $ if x1 > x2 then (VBool True) else (VBool False)

eval (GreaterEq _ e1 e2) =
    do { v1 <- eval e1
       ; v2 <- eval e2
       ; case (v1, v2) of
             (VInt x1, VInt x2)     -> evalEquality x1 x2
             (VFloat x1, VFloat x2) -> evalEquality x1 x2
             (VChar x1, VChar x2)   -> evalEquality x1 x2
             (VBool x1, VBool x2)   -> evalEquality x1 x2
             (VChars x1, VChars x2) -> evalEquality x1 x2
             _                      -> error "Illegal State: Comparison performed on wrong values!"
       }
    where evalEquality x1 x2 = return $ if x1 >= x2 then (VBool True) else (VBool False)

eval (Equal _ e1 e2) =
    do { v1 <- eval e1
       ; v2 <- eval e2
       ; case (v1, v2) of
             (VInt x1, VInt x2)     -> evalEquality x1 x2
             (VFloat x1, VFloat x2) -> evalEquality x1 x2
             (VChar x1, VChar x2)   -> evalEquality x1 x2
             (VBool x1, VBool x2)   -> evalEquality x1 x2
             (VChars x1, VChars x2) -> evalEquality x1 x2
             _                      -> error "Illegal State: Comparison performed on wrong values!"
       }
    where evalEquality x1 x2 = return $ if x1 == x2 then (VBool True) else (VBool False)

eval (NotEqual _ e1 e2) =
    do { v1 <- eval e1
       ; v2 <- eval e2
       ; case (v1, v2) of
             (VInt x1, VInt x2)     -> evalInequality x1 x2
             (VFloat x1, VFloat x2) -> evalInequality x1 x2
             (VChar x1, VChar x2)   -> evalInequality x1 x2
             (VBool x1, VBool x2)   -> evalInequality x1 x2
             (VChars x1, VChars x2) -> evalInequality x1 x2
             _                      -> error "Illegal State: Comparison performed on wrong values!"
       }
    where evalInequality x1 x2 = return $ if x1 /= x2 then (VBool True) else (VBool False)

eval (And t e1 e2) = undefined

eval (Or t e1 e2) = undefined

eval f@(Func _ _ ident _ _) =
    do { env <- get
       ; let v = Closure f env -- This enables lexical scoping of functions
       ; storeIdentInTopScope ident v env
       ; return v
       }

eval (Define _ ident e) =
    do { v <- eval e
       ; env <- get
       ; storeIdentInTopScope ident v env
       ; return v
       }

eval (Lit _ v) = return v

eval (Block _ es) =
    do { pushScope
       ; vs <- mapM (\e -> eval e) es
       ; popScope
       ; if null vs
         then error "Illegal State: Block expressions must contain at least one expression!"
         else return $ last vs
       }

eval (Cond _ e1 e2 e3) =
    do { v <- eval e1
       ; case v of
             VBool True  -> eval e2
             VBool False -> eval e3
             _           -> error "Illegal State: the condition in the if expr must be a boolean!"
       }

eval (Loop t e body) = undefined

eval (Input t) = liftIO getLine >>= (\input -> evalInput input t) -- or lift . lift instead of liftIO

eval (Print _ args) =
    do { vs <- mapM (\arg -> eval arg) args
       ; mapM (\v -> printValue v) vs
       ; return VNull
       }

eval (Call _ ident args) =
    do { env <- get
       ; case inScope ident env of
             Just closure@(Closure (Func _ _ _ params body) closureEnv) ->
                 do { vs <- mapM (\arg -> eval arg) args
                    ; put closureEnv -- Switch to the env captured when the function was evaluated
                    ; pushScope
                    ; extendEnvWithParams (zip params vs)
                    ; extendedClosureEnv <- get
                    ; storeIdentInTopScope ident closure extendedClosureEnv
                    ; returnValue <- eval body
                    ; popScope
                    ; put env -- Switch back to the env where the call was evaluated
                    ; return returnValue
                    }
             _                                                          -> error "Illegal State: Function not in scope!"
       }

eval (Assign _ ident e) =
    do { v <- eval e
       ; env <- get
       ; updateVariable ident v env
       ; return v
       }

eval (Var _ ident) =
    do { env <- get
       ; case inScope ident env of
             Nothing            -> error "Illegal State: Variable not in scope!"
             Just (Closure _ _) -> error "Illegal State: Variable not in scope!"
             Just v             -> return v
       }

evalInput :: String -> Type -> Interpreter Val
evalInput input TInt =
    case (readMaybe input :: Maybe Integer) of
        Nothing  -> throwError $ InputError "Input is not an integer"
        Just v   -> return $ VInt v
evalInput input TFloat =
    case (readMaybe input :: Maybe Double) of
        Nothing  -> throwError $ InputError "Input is not a float"
        Just v   -> return $ VFloat v
evalInput input TChar = error "Illegal State: Cannot read a character!"
evalInput input TBool = error "Illegal State: Cannot read a boolean!"
evalInput input TInts = error "Illegal State: Cannot read a list of integers!"
evalInput input TFloats = error "Illegal State: Cannot read a list of floats!"
evalInput input TChars = return $ VChars input
evalInput input TBools = error "Illegal State: Cannot read a list of booleans!"
evalInput input TVoid = error "Illegal State: Cannot read a null!"
evalInput input (Sig _ _) = error "Illegal State: Cannot read a function!"

printValue :: Val -> Interpreter ()
printValue v =
    case v of
        VInt x      -> liftIO ((putStr . show) x >> hFlush stdout >> return ())
        VFloat x    -> liftIO ((putStr . show) x >> hFlush stdout >> return ())
        VChar x     -> liftIO (putChar x >> hFlush stdout >> return ())
        VBool x     -> liftIO ((putStr . show) x >> hFlush stdout >> return ())
        VInts x     -> liftIO ((putStr . show) x >> hFlush stdout >> return ())
        VFloats x   -> liftIO ((putStr . show) x >> hFlush stdout >> return ())
        VChars x    -> liftIO (putStr x >> hFlush stdout >> return ())
        VBools x    -> liftIO ((putStr . show) x >> hFlush stdout >> return ())
        VNull       -> liftIO (putStr "null" >> hFlush stdout >> return ())
        Closure _ _ -> throwError AttemptToPrintFunc

-- Extract value of identifier from the environment starting from the inner most/top scope,
-- stopping when you encounter one (this allows variable shadowing),
-- returning Nothing if it's absent
inScope :: Ident -> DynamicEnv -> Maybe Val
inScope ident []             = Nothing
inScope ident (scope:scopes) =
    case ident `M.lookup` scope of
        Nothing -> inScope ident scopes
        v       -> v

-- Extract value of identifier from the top scope, return Nothing if it's absent
inTopScope :: Ident -> DynamicEnv -> Maybe Val
inTopScope ident [] = error "Illegal State: Looking at an empty environment!"
inTopScope ident (scope:_) = ident `M.lookup` scope

-- Store identifier with a value in the top scope of environment
storeIdentInTopScope :: Ident -> Val -> DynamicEnv -> Interpreter ()
storeIdentInTopScope _ _ [] = error "Illegal State: Storing variable to empty environment!"
storeIdentInTopScope ident v (scope:scopes) = put ((M.insert ident v scope):scopes)

-- Update value of identifier in the environment starting from the inner most/top scope,
-- stopping when you encounter one (this allows variable shadowing)
updateVariable :: Ident -> Val -> DynamicEnv -> Interpreter ()
updateVariable _ _ [] = error "Illegal State: Updating variable to empty environment!"
updateVariable ident v (scope:scopes) =
    if updatedScope == scope -- Means it hasn't found the variable in that scope
    then do { updateVariable ident v scopes
            ; updatedScopes <- get
            ; put (scope:updatedScopes)
            }
    else put (updatedScope:scopes)
    where updatedScope = M.adjust (\_ -> v) ident scope

-- Push a new scope to the top
pushScope :: Interpreter ()
pushScope =
    do { env <- get
       ; put (M.empty:env)
       }

-- Pop the scope at the top
popScope :: Interpreter ()
popScope =
    do { env <- get
       ; case env of
             [] -> error "Illegal State: Popping a scope from an empty environment!"
             (_:scopes) -> put scopes
       }

-- Store parameter identifiers with their values in the top scope of environment
extendEnvWithParams :: [(Param, Val)] -> Interpreter ()
extendEnvWithParams [] = return ()
extendEnvWithParams (((Param _ ident), v):paramArgPairs) =
    do { env <- get
       ; storeIdentInTopScope ident v env
       ; extendEnvWithParams paramArgPairs
       }
