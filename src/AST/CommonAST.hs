module AST.CommonAST where

import qualified Data.Map.Strict as Map (Map)

type Env = [Map.Map String Type]

type Ident = String

-- Function signature [t0, t1, ... , tn] means the function has return type t0 and parameter types t1 to tn
data Type = TInt | TFloat | TChar | TBool | TInts | TFloats | TChars | TBools | TVoid | Sig [Type]
    deriving (Show, Eq)

data Param = Param Type Ident
    deriving (Show, Eq)
