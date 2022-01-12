module AST where

data Prog = Prog [Expr]
    deriving Show

data Expr = Var Ident
          | Define Type Ident Expr
          | Func Type Ident [Param] Expr
          | Cond Expr Expr Expr
          | Loop Expr Expr
          | Not Expr -- | Neg Expr
          | And Expr Expr | Or Expr Expr
          | Is Expr Expr | Isnot Expr Expr
          | Add Expr Expr | Minus Expr Expr | Mult Expr Expr | Div Expr Expr
          | Lesser Expr Expr | LesserEq Expr Expr
          | Greater Expr Expr | GreaterEq Expr Expr
          | Equal Expr Expr | NotEqual Expr Expr
          | Lit Val
          | Block [Expr]
          | Call Ident [Arg]
          | Assign Ident Expr
    deriving Show

data Val = VInt Int | VChar Char | VBool Bool | VInts [Int] | VChars [Char] | VBools [Bool] | VNull
    deriving Show

data Type = TInt | TChar | TBool | TInts | TChars | TBools | TVoid
    deriving Show

type Ident = String

data Param = Param Type Ident
    deriving Show

data Arg = Arg Expr
    deriving Show

program :: Prog
program =
    Prog [
        Define TInt "base" (Lit (VInt 1)),
        Func TInt "factorial" [Param TInt "num"] (Block [
            Cond (Equal (Var "num") (Var "base"))
            (Var "base")
            (Mult (Var "num") (Call "factorial" [Arg (Minus (Var "num") (Lit (VInt 1)))]))
        ]),
        Define TInt "result" (Lit (VInt (-1))),
        Assign "result" (Call "factorial" [Arg (Lit (VInt 5))])
    ]

{-

Grammar

prog ::= (expr ";")*
expr ::= var | define | func | cond | loop | op | lit | block | call | assign | "(" expr ")"
var ::= ident
ident ::= [_a-zA-Z][_a-zA-Z0-9]*
define ::= type ident "=" expr
func ::= type ident "(" params ")" expr
type ::= "int" | "char" | "bool" | "int[]" | "char[]" | "bool[]" | "void"
params ::= ( type ident ("," type ident)* ) | <empty>
cond ::= "if" "(" expr ")" expr "else" expr
loop ::= "while" "(" expr ")" expr
op ::= "not" expr | "~" expr
     | expr "and" expr | expr "or" expr
     | expr "is" expr | expr "isnot" expr
     | expr "+" expr | expr "-" expr | expr "*" expr | expr "/" expr
     | expr "<" expr | expr "<=" expr
     | expr ">" expr | expr ">=" expr
     | expr "==" expr | expr "!=" expr
lit ::= integer | character | boolean | array | string | null
interger ::= \d+
characters ::= "'" \w+ "'"
boolean ::= "true" | "false"
array ::= "[" ( (lit ("," lit)*) | <empty> ) "]"
string ::= "\"" [^"] "\""
null ::= "null"
block ::= "{" (expr ";")+ "}"
call ::= ident "(" args ")"
args ::= ( expr ("," expr)* ) | <empty>
assign ::= ident "=" expr

-}
