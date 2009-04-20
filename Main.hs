module Main where
import System.Environment
import Parser
import Eval

-- This file contains the main loop of the interpreter

main :: IO ()
main = getArgs >>= putStrLn . show . eval . parseScheme . head