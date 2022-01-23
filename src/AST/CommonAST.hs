module AST.CommonAST where

type Ident = String

data Type = TInt | TFloat | TChar | TBool | TInts | TFloats | TChars | TBools | TVoid | Sig Type [Type]
    deriving (Show, Eq)

data Param = Param Type Ident
    deriving (Show, Eq)
