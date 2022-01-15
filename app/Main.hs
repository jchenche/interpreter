module Main where

import Lib
import AST
import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as P
import Text.Parsec.Expr

prog :: GenParser Char st Prog
prog = Prog <$> (whiteSpace *> many (expr <* semi) <* eof)

expr = buildExpressionParser table term <?> "expression"

table = [ [prefix "!" Not, prefix "-" Neg, prefix "+" id]
        , [binary "*" Mult AssocLeft, binary "/" Div AssocLeft]
        , [binary "+" Add AssocLeft, binary "-" Minus AssocLeft]
        , [binary "<" Lesser AssocLeft, binary "<=" LesserEq AssocLeft, binary ">" Greater AssocLeft, binary ">=" GreaterEq AssocLeft]
        , [binary "==" Equal AssocLeft, binary "!=" NotEqual AssocLeft]
        , [binary "&&" And AssocLeft]
        , [binary "||" Or AssocLeft]
        ]

binary  name fun assoc = Infix (do{ reservedOp name; return fun }) assoc
prefix  name fun       = Prefix (do{ reservedOp name; return fun })
postfix name fun       = Postfix (do{ reservedOp name; return fun })

term = define
   <|> lit
   <|> block
   <|> try func
   <|> parens expr
   <|> cond
   <|> loop
   <|> try call
   <|> try assign
   <|> var
   <?> "term"

var = Var <$> identifier

define = do
    { t <- parseType
    ; i <- identifier
    ; reserved keywordAssign
    ; e <- expr
    ; return $ Define t i e
    }

parseType = TInts <$ reserved keywordInts
        <|> TChars <$ reserved keywordChars
        <|> TBools <$ reserved keywordBools
        <|> TInt <$ reserved keywordInt
        <|> TChar <$ reserved keywordChar
        <|> TBool <$ reserved keywordBool
        <|> TVoid <$ reserved keywordVoid
        <?> "type"

-- Give void as the type for now and replace it during type-checking (maybe not needed)
func = do
    { ps <- parens params
    ; reserved keywordArrow
    ; e <- expr
    ; return $ Func TVoid ps e
    }

params = commaSep (Param <$> parseType <*> identifier)

cond = do
    { reserved keywordIf
    ; condition <- parens expr
    ; e1 <- expr
    ; reserved keywordElse
    ; e2 <- expr
    ; return $ Cond condition e1 e2
    }

loop = do
    { reserved keywordWhile
    ; condition <- parens expr
    ; e <- expr
    ; return $ Loop condition e
    }

block = Block <$> braces (many1 (expr <* semi))

call = Call <$> identifier <*> parens (commaSep expr)

assign = do
    { i <- identifier
    ; reserved keywordAssign
    ; e <- expr
    ; return $ Assign i e
    }

lit = Lit . VInt <$> integer
  <|> Lit . VChar <$> charLiteral
  <|> (Lit $ VBool True) <$ reserved keywordTrue
  <|> (Lit $ VBool False) <$ reserved keywordFalse
  <|> try (Lit . VInts <$> brackets (commaSep integer))
  <|> try (Lit . VChars <$> brackets (commaSep charLiteral))
  <|> Lit . VBools <$> brackets (commaSep (True <$ reserved keywordTrue <|> False <$ reserved keywordFalse))
  <|> Lit . VChars <$> stringLiteral
  <|> Lit VNull <$ reserved keywordNull
  <?> "literal"

-- Lexer
keywordInt = "int"
keywordChar = "char"
keywordBool = "bool"
keywordInts = "int[]"
keywordChars = "char[]"
keywordBools = "bool[]"
keywordVoid = "void"
keywordIf = "if"
keywordElse = "else"
keywordWhile = "while"
keywordTrue = "true"
keywordFalse = "false"
keywordNull = "null"
keywordAssign = "="
keywordArrow = "->"

customDef :: P.LanguageDef st
customDef =  P.LanguageDef {
    P.commentStart = "/*",
    P.commentEnd = "*/",
    P.commentLine = "//",
    P.nestedComments = True,
    P.identStart = letter <|> char '_',
    P.identLetter = alphaNum <|> char '_',
    P.opStart = oneOf ":!#$%&*+./<=>?@\\^|-~",
    P.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~",
    P.reservedNames = [keywordInt, keywordChar, keywordBool, keywordInts, keywordChars, keywordBools, keywordVoid, keywordIf, keywordElse, keywordWhile, keywordTrue, keywordFalse, keywordNull, keywordAssign, keywordArrow],
    P.reservedOpNames = ["!" ,"*" ,"/" ,"+" ,"-" ,"<" ,"<=" ,">" ,">=" ,"==" ,"!=", "&&", "||"],
    P.caseSensitive = True
}

lexer = P.makeTokenParser customDef
identifier  = P.identifier lexer
integer = P.integer lexer
reserved = P.reserved lexer
semi = P.semi lexer
commaSep = P.commaSep lexer
parens = P.parens lexer
brackets = P.brackets lexer
braces = P.braces lexer
whiteSpace = P.whiteSpace lexer
charLiteral = P.charLiteral lexer
stringLiteral = P.stringLiteral lexer
reservedOp = P.reservedOp lexer

-- Entry point
-- parseProg = parse expr "Parse Error" "1_helloWorld"

main :: IO ()
main = do { result <- parseFromFile prog "test/playground.txt"
          ; case result of
                Left err  -> print err
                Right xs  -> print xs
          }
