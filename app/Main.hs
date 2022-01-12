module Main where

import Lib
import AST
import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef) -- maybe javaStyle
-- import Text.Parsec.Expr

-- prog :: GenParser Char st Prog

expr :: GenParser Char st Expr
expr = var
   <|> lit

var :: GenParser Char st Expr
var = Var <$> identifier

lit :: GenParser Char st Expr
lit = Lit . VInt <$> integer

-- Lexer
lexer = P.makeTokenParser emptyDef
identifier  = P.identifier lexer
integer = P.integer lexer

-- Entry point
-- parseProg = parse expr "Parse Error" "1_helloWorld"

main :: IO ()
main = do { result <- parseFromFile expr "test/playground.txt"
          ; case result of
                Left err  -> print err
                Right xs  -> print xs
          }
