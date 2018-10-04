module Main where

import Parser
import Tokenizer

runParser :: String -> IO ()
runParser input = do
  putStrLn input
  print $ parse input
  putStrLn ""

main :: IO ()
main = do
  runParser "1-2^-35*4"
  runParser "-(i = 3)"
  runParser " 1 * 2 - 3 / 4 + 5"
  runParser "-cat"
  runParser "(-x)^(-y)"
  runParser " 1 - 2 - 3 "
  runParser " (((9)))"
