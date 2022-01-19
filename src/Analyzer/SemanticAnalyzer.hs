module Analyzer.SemanticAnalyzer where

import AST.PlainAST
import qualified AST.TypedAST as T
import Control.Monad.State -- From the mtl library
-- import Control.Monad.Trans.State -- From the transformers library

typecheck :: Prog -> State Env T.Prog
typecheck = undefined
