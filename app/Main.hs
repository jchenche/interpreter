module Main where

import Lib
import AST
import Text.ParserCombinators.Parsec
-- import Text.ParserCombinators.Parsec.Expr



main :: IO ()
main = do
    contents <- readFile "test/program.txt"
    putStrLn contents
