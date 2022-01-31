module Main where

import Parser.Parser
import Analyzer.SemanticAnalyzer
import Interpreter.Interpreter
import System.Environment (getArgs)

main :: IO ()
main =
    do { args <- getArgs
       ; if length args /= 1
         then error "Must supply one file"
         else do { result <- parseSource (head args)
                 ; case result of
                       Left err  ->
                           putStr "\x1b[31m" >> putStr "Parse error: " >> print err >> putStr "\x1b[30m"
                       Right ast ->
                           do { case typeCheckAST ast of
                                    (Left err, env) ->
                                        putStr "\x1b[31m" >> putStr "Semantic error: " >> print err >>
                                        putStr "\x1b[30m" >> putStr "Environment: " >> print env
                                    (Right ast, _)  ->
                                        do { putStr "\x1b[32m" >> putStrLn "=== BEGIN EXECUTION ===" >> putStr "\x1b[30m"
                                           ; result <- interpretAST ast
                                           ; case result of
                                                 (Left err, env) ->
                                                     putStr "\x1b[31m" >> putStr "Runtime error: " >> print err >>
                                                     putStr "\x1b[30m" >> putStr "Environment: " >> print env
                                                 (Right _, _)    ->
                                                     putStr "\x1b[32m" >> putStrLn "=== SUCCESSFUL EXECUTION ===" >> putStr "\x1b[30m"
                                           }
                              }
                 }
       }
