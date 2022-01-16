module Parser.Lexer where

import Text.Parsec
import qualified Text.Parsec.Token as P

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
