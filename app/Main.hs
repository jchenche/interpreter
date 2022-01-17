module Main where

import Parser.Parser
import Text.Parsec.String (parseFromFile)
import Control.Monad.State -- From the mtl library
-- import Control.Monad.Trans.State -- From the transformers library

-- parseProg = parse expr "Parse Error" "1_helloWorld"
main :: IO ()
main = do { result <- parseFromFile prog "test/playground.txt"
          ; case result of
                Left err  -> print err
                Right xs  -> print xs
          }
