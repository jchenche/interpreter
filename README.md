# Interpreter for Gavagai
An interpreter for an imperative programming language, called Gavagai, I designed for fun and exploration.

## Instructions to run
### Must install Haskell Stack first (refer to https://docs.haskellstack.org/en/stable/README/)
`source buildScript.sh` to build the interpreter.  
`gavagai <source_filename>.txt` to interpret a program.

## Grammar
```
prog ::= (expr ";")*
expr ::= "!" expr | "-" expr | "+" expr
       | expr "*" expr | expr "/" expr
       | expr "+" expr | expr "-" expr
       | expr "<" expr | expr "<=" expr | expr ">" expr | expr ">=" expr
       | expr "==" expr | expr "!=" expr
       | expr "&&" expr
       | expr "||" expr
       | term
term ::= func
       | define
       | lit
       | block
       | "(" expr ")"
       | cond
       | loop
       | input
       | print
       | call
       | array_mod
       | array_access
       | assign
       | var
func ::= type ident "(" params ")" expr
define ::= type ident "=" expr
lit ::= integer | float | character | boolean | array | string | null
block ::= "{" (expr ";")+ "}"
cond ::= "if" "(" expr ")" expr "else" expr
loop ::= "while" "(" expr ")" expr
input ::= "input" type
print ::= "print" "(" args ")"
call ::= ident "(" args ")"
array_mod ::= ident "[" expr "->" expr "]"
array_access ::= ident "[" expr "]"
assign ::= ident "=" expr
var ::= ident
integer ::= \d+
float ::= \d+\.\d+
character ::= "'" [^'] "'"
boolean ::= "true" | "false"
array ::= "[" ( (lit ("," lit)*) | <empty> ) "]"
string ::= "\"" [^"]* "\""
null ::= "null"
ident ::= [_a-zA-Z][_a-zA-Z0-9]*
type ::= "int" | "float" | "char" | "bool" | "ints" | "floats" | "chars" | "bools" | "void"
params ::= ( type ident ("," type ident)* ) | <empty>
args ::= ( expr ("," expr)* ) | <empty>
```
