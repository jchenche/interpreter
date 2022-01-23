module Main where

import Parser.Parser
import Analyzer.SemanticAnalyzer
import Interpreter.Interpreter

main :: IO ()
main =
    do { result <- parseSource "test/program.txt"
       ; case result of
             Left err  -> print err
             Right ast -> do { print $ "Match AST: " ++ show (ast == program)
                             ; case typeCheckAST ast of
                                   (Left err, env) -> print err
                                   (Right ast, _)  -> do { print $ "Match Typed AST: " ++ show (ast == typedProgram)
                                                         ; result <- interpretAST ast
                                                         ; case result of
                                                               (Left err, env) -> print err
                                                               (Right _, _)    -> putStrLn "=== SUCCESSFUL EXECUTION ==="
                                                         }
                             }
       }

-- Note: during evaluation, fromIntegral is useful for arithmetic involving an integer and a float. M.adjust or M.alter (or just M.update) is useful in variable assignments.
-- TODO: define array access and modification
