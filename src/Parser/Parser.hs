module Parser.Parser (prog, program, playground) where

import AST.CommonAST
import AST.PlainAST
import Parser.Lexer
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
term = try func
   <|> define
   <|> lit
   <|> block
   <|> parens expr
   <|> cond
   <|> loop
   <|> try call
   <|> try assign
   <|> var
   <?> "term"

func = do
    { t <- parseType
    ; i <- identifier
    ; ps <- parens params
    ; e <- expr
    ; return $ Func t i ps e
    }

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

program :: Prog
program =
    Prog [Define TInt "base" (Lit (VInt 1))
        , Func TInt "factorial" [Param TInt "num"] 
            (Cond (Equal (Var "num") (Var "base"))
            (Var "base")
            (Mult (Var "num") (Call "factorial" [Minus (Var "num") (Lit (VInt 1))])))
        , Define TFloat "aFloat" (Minus (Mult (Add (Neg (Lit (VInt 1))) (Lit (VInt 0))) (Lit (VInt 3))) (Mult (Mult (Lit (VInt 4)) (Lit (VFloat 5.1))) (Lit (VInt 6))))
        , Define TInt "result" (Lit (VInt 10000000))
        , Assign "result" (Call "factorial" [Lit (VInt 5)])
    ]

playground :: Prog
playground =
    Prog [Define TInt "x" (Lit (VInt 3))
        , Func TInt "identFunc" [Param TInt "num"] (Block [Var "x", Mult (Add (Lit (VInt 1)) (Neg (Var "num"))) (Var "num")])
        , Define TBools "conds" (Lit (VBools [True,True,False,True]))
        , Loop (Lit (VBool True)) (Cond (Lit (VBool True)) (Add (Lit (VInt 1)) (Mult (Neg (Cond (Lit (VBool True)) (Var "x") (Var "x"))) (Var "x"))) (Lit (VInt 3)))
        , Define TChars "greet" (Lit (VChars "hello world"))
        , Lit VNull
    ]
