module Parser.Parser (prog) where

import Parser.Lexer
import AST
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String (GenParser)

prog :: GenParser Char st Prog
prog = Prog <$> (whiteSpace *> many (expr <* semi) <* eof)

expr :: GenParser Char st Expr
expr = buildExpressionParser opTable term <?> "expression"

opTable = [ [prefix "!" Not, prefix "-" Neg, prefix "+" id]
        , [binary "*" Mult AssocLeft, binary "/" Div AssocLeft]
        , [binary "+" Add AssocLeft, binary "-" Minus AssocLeft]
        , [binary "<" Lesser AssocLeft, binary "<=" LesserEq AssocLeft, binary ">" Greater AssocLeft, binary ">=" GreaterEq AssocLeft]
        , [binary "==" Equal AssocLeft, binary "!=" NotEqual AssocLeft]
        , [binary "&&" And AssocLeft]
        , [binary "||" Or AssocLeft]
        ]

binary name fun assoc = Infix (do{ reservedOp name; return fun }) assoc
prefix name fun = Prefix (do{ reservedOp name; return fun })

term :: GenParser Char st Expr
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

define = do
    { t <- parseType
    ; i <- identifier
    ; reserved keywordAssign
    ; e <- expr
    ; return $ Define t i e
    }

lit = Lit . VFloat <$> try float
  <|> Lit . VInt <$> integer
  <|> Lit . VChar <$> charLiteral
  <|> (Lit $ VBool True) <$ reserved keywordTrue
  <|> (Lit $ VBool False) <$ reserved keywordFalse
  <|> try (Lit . VInts <$> brackets (commaSep integer))
  <|> try (Lit . VFloats <$> brackets (commaSep float))
  <|> try (Lit . VChars <$> brackets (commaSep charLiteral))
  <|> Lit . VBools <$> brackets (commaSep (True <$ reserved keywordTrue <|> False <$ reserved keywordFalse))
  <|> Lit . VChars <$> stringLiteral
  <|> Lit VNull <$ reserved keywordNull
  <?> "literal"

block = Block <$> braces (many1 (expr <* semi))

-- Give void as the type for now and replace it during type-checking (maybe not needed)
func = do
    { ps <- parens params
    ; reserved keywordArrow
    ; e <- expr
    ; return $ Func TVoid ps e
    }

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

call = Call <$> identifier <*> parens (commaSep expr)

assign = do
    { i <- identifier
    ; reserved keywordAssign
    ; e <- expr
    ; return $ Assign i e
    }

var = Var <$> identifier

parseType = TInts <$ reserved keywordInts
        <|> TFloats <$ reserved keywordFloats
        <|> TChars <$ reserved keywordChars
        <|> TBools <$ reserved keywordBools
        <|> TInt <$ reserved keywordInt
        <|> TFloat <$ reserved keywordFloat
        <|> TChar <$ reserved keywordChar
        <|> TBool <$ reserved keywordBool
        <|> TVoid <$ reserved keywordVoid
        <?> "type"

params = commaSep (Param <$> parseType <*> identifier)
