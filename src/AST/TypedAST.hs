module AST.TypedAST where

import AST.CommonAST
import qualified Data.Map as Map (Map)

type Env = [Map.Map String Val]

data Prog = Prog [Expr]
    deriving (Show, Eq)

data Expr = Not Type Expr | Neg Type Expr
          | Mult Type Expr Expr | Div Type Expr Expr
          | Add Type Expr Expr | Minus Type Expr Expr
          | Lesser Type Expr Expr | LesserEq Type Expr Expr | Greater Type Expr Expr | GreaterEq Type Expr Expr
          | Equal Type Expr Expr | NotEqual Type Expr Expr
          | And Type Expr Expr
          | Or Type Expr Expr
          | Define Type Ident Expr
          | Lit Type Val
          | Block Type [Expr]
          | Func Type [Param] Expr
          | Cond Type Expr Expr Expr
          | Loop Type Expr Expr
          | Call Type Ident [Expr]
          | Assign Type Ident Expr
          | Var Type Ident
    deriving (Show, Eq)

data Val = VInt Integer | VFloat Double | VChar Char | VBool Bool | VInts [Integer] | VFloats [Double] | VChars [Char] | VBools [Bool] | VNull | Closure [Param] Expr Env
    deriving (Show, Eq)
