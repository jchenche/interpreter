-- {-# OPTIONS_GHC -Wall #-}
module Analyzer.SemanticAnalyzer where

import AST.CommonAST
import qualified AST.PlainAST as PT
import AST.TypedAST
import Control.Monad.State -- From the mtl library
-- import Control.Monad.Trans.State -- From the transformers library
import qualified Data.Map.Strict as Map

type StaticEnv = [Map.Map String Type]

programTypeChecker :: PT.Prog -> State StaticEnv Prog
programTypeChecker (PT.Prog es) =
    do { typedEs <- mapM (\e -> typec e) es
       ; return $ Prog typedEs
       }

typec :: PT.Expr -> State StaticEnv Expr -- TODO
typec (PT.Var i) = return $ Var TVoid i

typec (PT.Define t i e) = -- TODO
    do { typedE <- typec e
       ; env <- get
       ; return $ Define t i typedE
       }

typec (PT.Not e) = 
    do { typedE <- typec e
       ; return $ Not (getT typedE) typedE
       }

typec _ = error "Shuoldn't be here"

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
getT (Func t _ _ _) = t
getT (Cond t _ _ _) = t
getT (Loop t _ _) = t
getT (Call t _ _) = t
getT (Assign t _ _) = t
getT (Var t _) = t
