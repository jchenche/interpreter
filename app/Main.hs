module Main where

import Parser.Parser
import Text.Parsec.String (parseFromFile)
import Analyzer.SemanticAnalyzer
import Control.Monad.State (runState, runStateT)
import Control.Monad.Except (runExcept, runExceptT)

-- parseProg = parse expr "Parse Error" "1_helloWorld"
main :: IO ()
main =
    do { result <- parseFromFile prog "test/program.txt"
       ; case result of
             Left err -> print err
             Right xs -> do { print $ xs == program
                            ; print $ typedSimple == runState (runExceptT (programTypeChecker xs)) []
                            ; print $ runState (runExceptT (programTypeChecker xs)) []
                            }
       }

-- Note: during evaluation, fromIntegral is useful for arithmetic involving an integer and a float. M.adjust or M.alter (or just M.update) is useful in variable assignments.
-- TODO: define array access and modification
