module Main where

import Parser.Parser
import Text.Parsec.String (parseFromFile)
import Analyzer.SemanticAnalyzer
import Control.Monad.State (runState)

-- parseProg = parse expr "Parse Error" "1_helloWorld"
main :: IO ()
main =
    do { result <- parseFromFile prog "test/program.txt"
       ; case result of
             Left err -> print err
             Right xs -> do { print $ xs == program
                            ; print $ runState (programTypeChecker xs) []
                            }
       }

-- Note: during evaluation, fromIntegral is useful for arithmetic involving an integer and a float. M.adjust or M.alter (or just M.update) is useful in variable assignments.
