-- {-# OPTIONS_GHC -Wall #-}
module Analyzer.SemanticAnalyzer where

import AST.CommonAST
import qualified AST.PlainAST as PT
import AST.TypedAST
import Control.Monad.State -- From the mtl library
-- import Control.Monad.Trans.State -- From the transformers library
import qualified Data.Map.Strict as M

type StaticEnv = [M.Map String Type]

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
getT (Func t _ _ _ _) = t
getT (Cond t _ _ _) = t
getT (Loop t _ _) = t
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
