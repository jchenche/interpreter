module Main where

import Parser.Parser
import Text.Parsec.String (parseFromFile)
import Analyzer.SemanticAnalyzer
import Control.Monad.State (evalState)

-- parseProg = parse expr "Parse Error" "1_helloWorld"
main :: IO ()
main =
    do { result <- parseFromFile prog "test/program.txt"
       ; case result of
             Left err -> print err
             Right xs -> do { print $ xs == program
                            ; print $ evalState (programTypeChecker xs) []
                            }
       }

-- Note: during evaluation, use fromIntegral
