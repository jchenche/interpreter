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
        ]

binary  name fun assoc = Infix (do{ reservedOp name; return fun }) assoc
prefix  name fun       = Prefix (do{ reservedOp name; return fun })
postfix name fun       = Postfix (do{ reservedOp name; return fun })

term = var
   <|> try define
   <|> func
   <|> cond
   <|> loop
   <|> lit
   <|> parens expr
   <?> "term"

var = Var <$> identifier

define = do
    { t <- parseType
    ; i <- identifier
    ; symbol "="
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

func = do
    { t <- parseType
    ; i <- identifier
    ; ps <- parens params
    ; e <- expr
    ; return $ Func t i ps e
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
    P.reservedNames = [keywordInt, keywordChar, keywordBool, keywordInts, keywordChars, keywordBools, keywordVoid, keywordIf, keywordElse, keywordWhile, keywordTrue, keywordFalse, keywordNull],
    P.reservedOpNames = ["not", "!" ,"~" ,"and" ,"or" ,"is" ,"isnot" ,"+" ,"-" ,"*" ,"/" ,"<" ,"<=" ,">" ,">=" ,"==" ,"!="],
    P.caseSensitive = True
}

lexer = P.makeTokenParser customDef
identifier  = P.identifier lexer
integer = P.integer lexer
symbol = P.symbol lexer
reserved = P.reserved lexer
semi = P.semi lexer
semiSep = P.semiSep lexer
commaSep = P.commaSep lexer
parens = P.parens lexer
whiteSpace = P.whiteSpace lexer
charLiteral = P.charLiteral lexer
stringLiteral = P.stringLiteral lexer
brackets = P.brackets lexer
reservedOp = P.reservedOp lexer

-- Entry point
-- parseProg = parse expr "Parse Error" "1_helloWorld"

main :: IO ()
main = do { result <- parseFromFile prog "test/playground.txt"
          ; case result of
                Left err  -> print err
                Right xs  -> print xs
          }
