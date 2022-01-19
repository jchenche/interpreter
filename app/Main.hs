module Main where

import AST.PlainAST
import Parser.Parser
import Text.Parsec.String (parseFromFile)
import Analyzer.SemanticAnalyzer

-- parseProg = parse expr "Parse Error" "1_helloWorld"
main :: IO ()
main = do { result <- parseFromFile prog "test/program.txt"
          ; case result of
                Left err -> print err
                Right xs -> do
                            { return $ typecheck xs
                            ; print (xs == program)
                            }
          }

-- Note: during evaluation, use fromIntegral
