-- {-# OPTIONS_GHC -Wall #-}
module Analyzer.SemanticAnalyzer where

import AST.CommonAST
import qualified AST.PlainAST as PT
import AST.TypedAST
import Control.Monad.State -- From the mtl library
import Control.Monad.Identity
-- import Control.Monad.Trans.State -- From the transformers library
import qualified Data.Map.Strict as M

type StaticEnv = [M.Map Ident Type]

programTypeChecker :: PT.Prog -> StateT StaticEnv Identity Prog
programTypeChecker (PT.Prog es) =
    do { pushScope
       ; typedEs <- mapM (\e -> typec e) es
       ; return $ Prog typedEs
       }

typec :: PT.Expr -> StateT StaticEnv Identity Expr -- TODO
typec (PT.Var i) = return $ Var TVoid i

typec (PT.Define declaredType ident e) =
    do { typedE <- typec e
       ; env <- get
       ; if declaredType /= (getT typedE)
             then error "[TODO Change error handling] Type mismatch"
         else if inTopScope ident env
             then error "[TODO Change error handling] Redefinition error"
             else do { storeVariable ident (getT typedE) env
                     ; return $ Define declaredType ident typedE
                     }
       }

typec (PT.Not e) =
    do { typedE <- typec e
       ; return $ Not (getT typedE) typedE
       }

typec (PT.Lit e) =
    case e of
        PT.VInt v        -> return $Lit TInt (VInt v)
        PT.VFloat v      -> return $Lit TFloat (VFloat v)
        PT.VChar v       -> return $Lit TChar (VChar v)
        PT.VBool v       -> return $Lit TBool (VBool v)
        PT.VInts v       -> return $Lit TInts (VInts v)
        PT.VFloats v     -> return $Lit TFloats (VFloats v)
        PT.VChars v      -> return $Lit TChars (VChars v)
        PT.VBools v      -> return $Lit TBools (VBools v)
        PT.VNull         -> return $Lit TVoid VNull
        PT.Closure _ _ _ -> error "Illegal State: Closure doesn't exist during type-checking"

typec _ = error "Illegal State: Shouldn't be here!"

-- Search if an identifier exists in the environment starting at the top scope
inScope :: Ident -> StaticEnv -> Bool
inScope ident []             = False
inScope ident (scope:scopes)
    | ident `M.member` scope = True
    | otherwise              = inScope ident scopes

-- Search if an identifier exists in the current scope
inTopScope :: Ident -> StaticEnv -> Bool
inTopScope ident []        = False
inTopScope ident (scope:_) = ident `M.member` scope

storeVariable :: Ident -> Type -> StaticEnv -> StateT StaticEnv Identity ()
storeVariable ident t (scope:scopes) = put (M.insert ident t scope:scopes)
storeVariable _ _ [] = error "Illegal State: Storing variable to empty environment"


-- Push a new scope to the top
pushScope :: StateT StaticEnv Identity ()
pushScope =
    do { env <- get
       ; put (M.empty:env)
       }

-- Pop the scope at the top
popScope :: StateT StaticEnv Identity ()
popScope =
    do { env <- get
       ; case env of
             [] -> error "Illegal State: Popping a scope from an empty environment!"
             (_:scopes) -> put scopes
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

typedSimple = (Prog [Define TInt "x" (Lit TInt (VInt 1))
                   , Define TFloat "y" (Lit TFloat (VFloat 2.0))
                   , Not TVoid (Var TVoid "x")
                   , Var TVoid "y"
                    ]
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
