module Datatypes ( LispVal(..) ) where

-- Scheme Datatypes

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal -- Apparently a valid datatype in Scheme
             | Number Integer
             | Float Float
             | String String
             | Bool Bool
             | Char Char
instance Show LispVal where show = showVal

-- Display LispVals
showVal :: LispVal -> String
showVal (Atom name) = name
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (Number contents) = show contents
-- Need one for Float
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
-- Need one for Char

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal