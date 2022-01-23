module Interpreter.Interpreter where

import AST.CommonAST
import AST.TypedAST
import Control.Monad.State (StateT, runStateT, put, get)
import Control.Monad.Except (ExceptT, runExceptT, throwError, liftIO)
import System.IO (hFlush, stdout)
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
       ; put (M.empty:env)
       ; liftIO $ putStrLn "Put your name below..."
       ; liftIO $ putStr "Name: " >> hFlush stdout 
       ; input <- liftIO getLine
       ; liftIO $ print ("Hello " ++ input) -- or lift . lift $ print "hi"
       ; throwError ArrayOutOfBound
       ; return $ Prog [Lit TInt (VInt 3)]
       }

eval :: Expr -> Interpreter Expr

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

eval (Lit t v) = undefined

eval (Block t es) = undefined

eval (Cond t e1 e2 e3) = undefined

eval (Loop t e body) = undefined

eval (Call t ident args) = undefined

eval (Assign t ident e) = undefined

eval (Var t ident) = undefined
