module Parser.Parser (parseSource, program, playground) where

import AST.CommonAST
import AST.PlainAST
import Parser.Lexer
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String (GenParser, parseFromFile)
import Text.Parsec.Error (ParseError)

parseSource :: String -> IO (Either ParseError Prog)
parseSource sourceFile = parseFromFile prog sourceFile

prog :: GenParser Char st Prog
prog = Prog <$> (whiteSpace *> many (expr <* semi) <* eof)

expr :: GenParser Char st Expr
expr = buildExpressionParser opTable term <?> "expression"

opTable = [ [prefix "!" Not, prefix "-" Neg, prefix "+" id]
          , [binary "*" Mult AssocLeft, binary "/" Div AssocLeft]
          , [binary "+" Plus AssocLeft, binary "-" Minus AssocLeft]
          , [binary "<" Lesser AssocNone, binary "<=" LesserEq AssocNone, binary ">" Greater AssocNone, binary ">=" GreaterEq AssocNone]
          , [binary "==" Equal AssocNone, binary "!=" NotEqual AssocNone]
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
   <|> parseInput
   <|> parsePrint
   <|> try call
   <|> try arrayMod
   <|> try arrayAccess
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
    ; reservedOp keywordAssign
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

parseInput = Input <$> (reserved keywordInput *> parseType)

parsePrint = Print <$> (reserved keywordPrint *> parens (commaSep expr))

call = Call <$> identifier <*> parens (commaSep expr)

arrayMod = do
    { i <- identifier
    ; (e1, e2) <- brackets (do { e1 <- expr
                               ; reservedOp keywordArrow
                               ; e2 <- expr
                               ; return (e1, e2)
                               })
    ; return $ ArrayMod i e1 e2
    }

arrayAccess = ArrayAccess <$> identifier <*> brackets expr

assign = do
    { i <- identifier
    ; reservedOp keywordAssign
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
    Prog [Print [Lit (VChars "Beginning of program"), Lit (VChars "\n")]
        , Define TInt "base" (Lit (VInt 1))
        , Func TInt "factorial" [Param TInt "num"] (Cond (Equal (Var "num") (Var "base")) (Var "base") (Mult (Var "num") (Call "factorial" [Minus (Var "num") (Lit (VInt 1))])))
        , Define TFloat "aFloat" (Minus (Mult (Plus (Neg (Lit (VInt 1))) (Lit (VInt 0))) (Lit (VInt 3))) (Mult (Mult (Lit (VInt 4)) (Lit (VFloat 5.1))) (Lit (VInt 6))))
        , Define TInt "result" (Lit (VInt 10000000))
        , Assign "result" (Call "factorial" [Lit (VInt 5)])
        , Print [Var "base", Lit (VChars "\n"), Var "aFloat", Lit (VChars "\n"), Var "result", Lit (VChars "\n")]
        , Print [Lit (VChars "Enter your name: ")]
        , Define TChars "name" (Input TChars)
        , Print [Lit (VChars "Hi, "), Var "name", Lit (VChars "\n")]
        , Loop (LesserEq (Var "base") (Lit (VInt 3))) (Block [Print [Var "base", Lit (VChars " ")], Assign "base" (Plus (Var "base") (Lit (VInt 1)))])
        , Print [Lit (VChars "base after loop: "), Var "base", Lit (VChars "\n")]
        , Define TInts "fib5" (Lit (VInts [1,1,2,3,500]))
        , Print [Lit (VChars "Fib5 = "), Var "fib5", Lit (VChars "\n")]
        , Print [Lit (VChars "Fib5[4] = "), ArrayAccess "fib5" (Lit (VInt 4)), Lit (VChars "\n")]
        , ArrayMod "fib5" (Lit (VInt 4)) (Lit (VInt 5))
        , Print [Lit (VChars "Corrected Fib5 = "), Var "fib5", Lit (VChars "\n")]
        , Print [Lit (VChars "End of program"), Lit (VChars "\n")]
    ]

playground :: Prog
playground =
    Prog [Define TInt "x" (Lit (VInt 3))
        , Func TInt "identFunc" [Param TInt "num"] (Block [Var "x", Mult (Plus (Lit (VInt 1)) (Neg (Var "num"))) (Var "num")])
        , Define TBools "conds" (Lit (VBools [True,True,False,True]))
        , Loop (Lit (VBool True)) (Cond (Lit (VBool True)) (Plus (Lit (VInt 1)) (Mult (Neg (Cond (Lit (VBool True)) (Var "x") (Var "x"))) (Var "x"))) (Lit (VInt 3)))
        , Define TChars "greet" (Lit (VChars "hello world"))
        , Lit VNull
    ]
