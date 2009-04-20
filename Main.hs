module Main where
import System.Environment
import Parser

-- This file contains the main loop of the interpreter

main :: IO ()
main = do
  args <- getArgs
  putStrLn (parseScheme (args !! 0))