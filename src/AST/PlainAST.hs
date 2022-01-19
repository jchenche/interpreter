module AST.PlainAST where

type Env = [[(String, Val)]] -- Use Map.Map

data Prog = Prog [Expr]
    deriving (Show, Eq)

data Expr = Not Expr | Neg Expr
          | Mult Expr Expr | Div Expr Expr
          | Add Expr Expr | Minus Expr Expr
          | Lesser Expr Expr | LesserEq Expr Expr | Greater Expr Expr | GreaterEq Expr Expr
          | Equal Expr Expr | NotEqual Expr Expr
          | And Expr Expr
          | Or Expr Expr
          | Define Type Ident Expr
          | Lit Val
          | Block [Expr]
          | Func [Param] Expr
          | Cond Expr Expr Expr
          | Loop Expr Expr
          | Call Ident [Expr]
          | Assign Ident Expr
          | Var Ident
    deriving (Show, Eq)

data Val = VInt Integer | VFloat Double | VChar Char | VBool Bool | VInts [Integer] | VFloats [Double] | VChars [Char] | VBools [Bool] | VNull | Closure [Param] Expr Env
    deriving (Show, Eq)

type Ident = String

data Type = TInt | TFloat | TChar | TBool | TInts | TFloats | TChars | TBools | TVoid
    deriving (Show, Eq)

data Param = Param Type Ident
    deriving (Show, Eq)
