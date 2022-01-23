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

interpretAST :: Prog -> IO (Either RuntimeError Val, DynamicEnv)
interpretAST ast = runStateT (runExceptT (programEvaluator ast)) []

-- programEvaluator :: Prog -> Interpreter Val
-- programEvaluator (Prog es) =
--     do { env <- get
--        ; put (M.empty:env)
--        ; liftIO $ putStrLn "Put your name below..."
--        ; liftIO $ putStr "Name: " >> hFlush stdout
--        ; input <- liftIO getLine
--        ; liftIO $ print ("Hello " ++ input) -- or lift . lift $ print "hi"
--        ; throwError ArrayOutOfBound
--        ; return $ VInt 3
--        }

programEvaluator :: Prog -> Interpreter Val
programEvaluator (Prog es) =
    do { pushScope
       ; vs <- mapM (\e -> eval e) es
       ; return $ VInt 3
       }

eval :: Expr -> Interpreter Val

eval (Not t e) = undefined

eval (Neg t e) = undefined

eval (Mult t e1 e2) = undefined

eval (Div t e1 e2) = undefined

eval (Plus t e1 e2) = undefined

eval (Minus t e1 e2) = undefined

eval (Lesser t e1 e2) = undefined

eval (LesserEq t e1 e2) = undefined

eval (Greater t e1 e2) = undefined

eval (GreaterEq t e1 e2) = undefined

eval (Equal t e1 e2) = undefined

eval (NotEqual t e1 e2) = undefined

eval (And t e1 e2) = undefined

eval (Or t e1 e2) = undefined

eval (Func t returnType ident params body) = undefined

eval (Define declaredType ident e) = undefined

eval (Lit t v) = return v

eval (Block t es) = undefined

eval (Cond t e1 e2 e3) = undefined

eval (Loop t e body) = undefined

eval (Input t) = liftIO getLine >>= (\input -> evalInput input t)

eval (Print t args) =
    do { vs <- mapM (\arg -> eval arg) args
       ; mapM (\v -> printValue v) vs
       ; return VNull
       }

eval (Call t ident args) = undefined

eval (Assign t ident e) = undefined

eval (Var t ident) = undefined

{-
    do {
       ;
       }
-}

evalInput :: String -> Type -> Interpreter Val
evalInput input TInt =
    case (readMaybe input :: Maybe Integer) of
        Nothing  -> throwError $ InputError "Input is not an integer"
        Just v   -> return $ VInt v
evalInput input TFloat =
    case (readMaybe input :: Maybe Double) of
        Nothing  -> throwError $ InputError "Input is not a float"
        Just v   -> return $ VFloat v
evalInput input TChar = error "Illegal State: cannot read a character!"
evalInput input TBool = error "Illegal State: cannot read a boolean!"
evalInput input TInts = error "Illegal State: cannot read a list of integers!"
evalInput input TFloats = error "Illegal State: cannot read a list of floats!"
evalInput input TChars = return $ VChars input
evalInput input TBools = error "Illegal State: cannot read a list of booleans!"
evalInput input TVoid = error "Illegal State: cannot read a null!"
evalInput input (Sig _ _) = error "Illegal State: cannot read a function!"

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

-- -- Store parameter identifiers with their values in the top scope of environment
-- extendEnvWithParam :: [Param] -> Interpreter ()
-- extendEnvWithParam [] = return ()
-- extendEnvWithParam ((Param t ident):params) =
--     do { env <- get
--        ; case inTopScope ident env of
--              Just _  -> throwError $ ParamConflict ident
--              Nothing -> do { storeIdentInTopScope ident t env
--                            ; extendEnvWithParam params
--                            }
--        }
