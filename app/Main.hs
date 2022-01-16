module Main where

import Parser.Parser
import Text.Parsec.String (parseFromFile)

-- parseProg = parse expr "Parse Error" "1_helloWorld"
main :: IO ()
main = do { result <- parseFromFile prog "test/program.txt"
          ; case result of
                Left err  -> print err
                Right xs  -> print xs
          }
