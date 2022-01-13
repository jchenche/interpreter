module Main where

import Lib
import AST
import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as P
-- import Text.Parsec.Expr

prog :: GenParser Char st Prog
prog = Prog <$> (whiteSpace *> many (expr <* semi) <* eof)

expr = var
   <|> try define
   <|> lit
   <|> func

var = Var <$> identifier

define = do
    { t <- parseType
    ; i <- identifier
    ; symbol "="
    ; e <- expr
    ; return $ Define t i e
    }

parseType = TInt <$ reserved "int"
        <|> TChar <$ reserved "char"
        <|> TBool <$ reserved "bool"
        <|> TInts <$ reserved "int[]"
        <|> TChars <$ reserved "char[]"
        <|> TBools <$ reserved "bool[]"
        <|> TVoid <$ reserved "void"

func = do
    { t <- parseType
    ; i <- identifier
    ; ps <- parens params
    ; e <- expr
    ; return $ Func t i ps e
    }

params = commaSep (Param <$> parseType <*> identifier)

lit = Lit . VInt <$> integer

-- Lexer
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
    P.reservedNames = ["int", "char", "bool", "int[]", "char[]", "bool[]", "void", "if", "else", "while", "true", "false", "null"],
    P.reservedOpNames = ["not" ,"~" ,"and" ,"or" ,"is" ,"isnot" ,"+" ,"-" ,"*" ,"/" ,"<" ,"<=" ,">" ,">=" ,"==" ,"!="],
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

-- Entry point
-- parseProg = parse expr "Parse Error" "1_helloWorld"

main :: IO ()
main = do { result <- parseFromFile prog "test/playground.txt"
          ; case result of
                Left err  -> print err
                Right xs  -> print xs
          }
