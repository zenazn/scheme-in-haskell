module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Monad

-- Datatypes
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal -- Apparently a valid datatype in Scheme
             | Number Integer
             | String String
             | Bool Bool

-- Parsers
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

escapeChar :: Parser Char
escapeChar = do char '\\'
                esc <- anyChar
                return $ case esc of
                           'n' -> '\n'
                           'r' -> '\r'
                           't' -> '\t'
                           otherwise -> esc
             <|> noneOf "\""


parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (escapeChar <|> noneOf "\"")
                 char '"'
                 return $ String x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
                          otherwise -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber


-- Parsing logic
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> "No match: " ++ show err
                   Right val -> case val of
                                  String s -> "String: " ++ show s
                                  otherwise -> "Found value"

-- Main Loop
main :: IO ()
main = do
  args <- getArgs
  putStrLn (readExpr (args !! 0))