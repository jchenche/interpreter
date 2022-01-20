# Interpreter for [name of the language to be TBD]

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
       | call
       | assign
       | var
func ::= type ident "(" params ")" expr
define ::= type ident "=" expr
lit ::= integer | float | character | boolean | array | string | null
block ::= "{" (expr ";")+ "}"
cond ::= "if" "(" expr ")" expr "else" expr
loop ::= "while" "(" expr ")" expr
call ::= ident "(" args ")"
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
type ::= "int" | "float" | "char" | "bool" | "int[]" | "float[]" | "char[]" | "bool[]" | "void"
params ::= ( type ident ("," type ident)* ) | <empty>
args ::= ( expr ("," expr)* ) | <empty>
```
