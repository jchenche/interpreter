module Main where

import Parser.Parser
import Analyzer.SemanticAnalyzer
import Interpreter.Interpreter

main :: IO ()
main =
    do { result <- parseSource "test/program.txt"
       ; case result of
             Left err  -> print err
             Right ast -> do { case typeCheckAST ast of
                                   (Left err, env) -> print err >> print env
                                   (Right ast, _)  -> do { putStrLn "=== BEGIN EXECUTION ==="
                                                         ; result <- interpretAST ast
                                                         ; case result of
                                                               (Left err, env) -> print err >> print env
                                                               (Right _, _)    -> putStrLn "=== SUCCESSFUL EXECUTION ==="
                                                         }
                             }
       }
