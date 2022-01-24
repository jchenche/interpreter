module Main where

import Parser.Parser
import Analyzer.SemanticAnalyzer
import Interpreter.Interpreter

main :: IO ()
main =
    do { result <- parseSource "test/testEval.txt"
       ; case result of
             Left err  -> print err
             Right ast -> do { print $ "Match Plain AST: " ++ show (True)
                             ; case typeCheckAST ast of
                                   (Left err, env) -> print err
                                   (Right ast, _)  -> do { print $ "Match Typed AST: " ++ show (True)
                                                         ; putStrLn "=== BEGIN EXECUTION ==="
                                                         ; result <- interpretAST ast
                                                         ; case result of
                                                               (Left err, env) -> print err
                                                               (Right _, env)    -> putStrLn "=== SUCCESSFUL EXECUTION ===" >> print env
                                                         }
                             }
       }

-- TODO: define array access and modification
