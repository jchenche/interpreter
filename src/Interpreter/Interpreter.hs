module Interpreter.Interpreter where

import AST.CommonAST
import AST.TypedAST
import Control.Monad.State (StateT, runStateT, put, get) -- From the mtl library
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import qualified Data.Map.Strict as M

data RuntimeError = DivByZero
                  | ArrayOutOfBound
    deriving Eq

instance Show RuntimeError where
    show DivByZero = "Attempt to divide by zero"
    show _ = "[TODO] Runtime error, refine later"

type Interpreter = ExceptT RuntimeError (StateT DynamicEnv IO)

-- temp :: PT.Prog -> TypeChecker Prog
-- temp (PT.Prog es) =
--     do {
--          env <- get
--     --    ; lift $ lift $ print "hi"
--        ; put (M.empty:env)
--        ; throwError (FuncNotInScope "hello")
--        ; return $ Prog [Lit TInt (VInt 3)]
--        }

interpretAST :: Prog -> IO (Either RuntimeError Prog, DynamicEnv)
interpretAST ast = return (Left DivByZero, [])
