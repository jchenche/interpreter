module Interpreter.Interpreter where

import AST.CommonAST
import AST.TypedAST
import Control.Monad.State (StateT, runStateT, put, get, liftIO) -- From the mtl library
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import qualified Data.Map.Strict as M

data RuntimeError = DivByZero
                  | ArrayOutOfBound
    deriving Eq

instance Show RuntimeError where
    show DivByZero = "Attempt to divide by zero"
    show _ = "[TODO] Runtime error, refine later"

type Interpreter = ExceptT RuntimeError (StateT DynamicEnv IO)

interpretAST :: Prog -> IO (Either RuntimeError Prog, DynamicEnv)
interpretAST ast = runStateT (runExceptT (programEvaluator ast)) []

programEvaluator :: Prog -> Interpreter Prog
programEvaluator (Prog es) =
    do { env <- get
       ; liftIO $ print "hello world!" -- or lift . lift $ print "hi"
       ; put (M.empty:env)
       ; throwError ArrayOutOfBound
       ; return $ Prog [Lit TInt (VInt 3)]
       }
