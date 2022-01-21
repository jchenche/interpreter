module AST.CommonAST where

type Ident = String

-- Function signature [t0, t1, ... , tn] means the function has return type t0 and parameter types t1 to tn
data Type = TInt | TFloat | TChar | TBool | TInts | TFloats | TChars | TBools | TVoid | Sig Type [Type]
    deriving (Show, Eq)

data Param = Param Type Ident
    deriving (Show, Eq)

data ProgramError = TypeMismatch Type Type
                  | VarNotInScope Ident
                  | FuncNotInScope Ident
                  | VarConflict Ident
                  | FuncConflict Ident
    deriving Eq

instance Show ProgramError where
    show (TypeMismatch t1 t2) = show t1 ++ "doesn't match" ++ show t2
    show _ = "[TODO] Error, refine later"
