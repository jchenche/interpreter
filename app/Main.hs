module Main where

import Parser.Parser
import Text.Parsec.String (parseFromFile)
import Analyzer.SemanticAnalyzer

-- parseProg = parse expr "Parse Error" "1_helloWorld"
main :: IO ()
main =
    do { result <- parseFromFile prog "test/program.txt"
       ; case result of
             Left err  -> print err
             Right ast -> do { print $ "Match AST: " ++ show (ast == program)
                             ; print $ "Match Typed AST: " ++ show (typedProgram == typeCheckAST ast)
                             ; print $ typeCheckAST ast
                             }
       }

-- Note: during evaluation, fromIntegral is useful for arithmetic involving an integer and a float. M.adjust or M.alter (or just M.update) is useful in variable assignments.
-- TODO: define array access and modification
