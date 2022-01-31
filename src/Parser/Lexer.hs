module Parser.Lexer where

import Text.Parsec
import qualified Text.Parsec.Token as P

keywordInt = "int"
keywordFloat = "float"
keywordChar = "char"
keywordBool = "bool"
keywordInts = "int[]"
keywordFloats = "float[]"
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
keywordInput = "input"
keywordPrint = "print"

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
    P.reservedNames =
        [
            keywordInt, keywordFloat, keywordChar, keywordBool, 
            keywordInts, keywordFloats, keywordChars, keywordBools, keywordVoid,
            keywordIf, keywordElse, keywordWhile, keywordTrue, keywordFalse, keywordNull,
            keywordAssign, keywordArrow, keywordInput, keywordPrint
        ],
    P.reservedOpNames =
        [
            "!" ,"*" ,"/" ,"+" ,"-" ,"<" ,"<=" ,">" ,">=" ,"==" ,"!=", "&&", "||"
        ],
    P.caseSensitive = True
}

lexer = P.makeTokenParser customDef

identifier  = P.identifier lexer
integer = P.integer lexer
float = P.float lexer
reserved = P.reserved lexer
reservedOp = P.reservedOp lexer
whiteSpace = P.whiteSpace lexer
charLiteral = P.charLiteral lexer
stringLiteral = P.stringLiteral lexer
semi = P.semi lexer
commaSep = P.commaSep lexer
parens = P.parens lexer
brackets = P.brackets lexer
braces = P.braces lexer
