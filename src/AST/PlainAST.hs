module AST.PlainAST where

import AST.CommonAST
import qualified Data.Map.Strict as Map (Map)

type DynamicEnv = [Map.Map String Val]

data Prog = Prog [Expr]
    deriving (Show, Eq)

data Expr = Not Expr | Neg Expr
          | Mult Expr Expr | Div Expr Expr
          | Add Expr Expr | Minus Expr Expr
          | Lesser Expr Expr | LesserEq Expr Expr | Greater Expr Expr | GreaterEq Expr Expr
          | Equal Expr Expr | NotEqual Expr Expr
          | And Expr Expr
          | Or Expr Expr
          | Func Type Ident [Param] Expr
          | Define Type Ident Expr
          | Lit Val
          | Block [Expr]
          | Cond Expr Expr Expr
          | Loop Expr Expr
          | Call Ident [Expr]
          | Assign Ident Expr
          | Var Ident
    deriving (Show, Eq)

data Val = VInt Integer | VFloat Double | VChar Char | VBool Bool | VInts [Integer] | VFloats [Double] | VChars [Char] | VBools [Bool] | VNull | Closure [Param] Expr DynamicEnv
    deriving (Show, Eq)
