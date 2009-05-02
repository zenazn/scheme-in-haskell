module Main where
import System.Environment
import Parser
import Eval
import Datatypes
import Monad(liftM)

-- This file contains the main loop of the interpreter

main :: IO ()
main = do
  args <- getArgs
  evaled <- return $ liftM show $ parseScheme (args !! 0) >>= eval
  putStrLn $ extractValue $ trapError evaled