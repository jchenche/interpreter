module AST.TypedAST where

import AST.CommonAST
import qualified Data.Map.Strict as M (Map)

type DynamicEnv = [M.Map Ident Val]

data Prog = Prog [Expr]
    deriving (Show, Eq)

data Expr = Not Type Expr | Neg Type Expr
          | Mult Type Expr Expr | Div Type Expr Expr
          | Plus Type Expr Expr | Minus Type Expr Expr
          | Lesser Type Expr Expr | LesserEq Type Expr Expr | Greater Type Expr Expr | GreaterEq Type Expr Expr
          | Equal Type Expr Expr | NotEqual Type Expr Expr
          | And Type Expr Expr
          | Or Type Expr Expr
          | Func Type Type Ident [Param] Expr -- First Type is the signature
          | Define Type Ident Expr
          | Lit Type Val
          | Block Type [Expr]
          | Cond Type Expr Expr Expr
          | Loop Type Expr Expr
          | Input Type
          | Print Type [Expr]
          | Call Type Ident [Expr]
          | Assign Type Ident Expr
          | Var Type Ident
    deriving (Show, Eq)

data Val = VInt Integer | VFloat Double | VChar Char | VBool Bool | VInts [Integer] | VFloats [Double] | VChars [Char] | VBools [Bool] | VNull | Closure [Param] Expr DynamicEnv
    deriving (Show, Eq)
