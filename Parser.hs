module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric(readInt, readDec, readOct, readHex)
import Char (digitToInt)

-- Datatypes
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal -- Apparently a valid datatype in Scheme
             | Number Integer
             | String String
             | Bool Bool
               deriving (Show)

-- Supporting Functions

readBin :: (Integral a) => ReadS a
readBin = readInt 2 (\c -> c == '0' || c == '1') digitToInt

-- Parsers
symbol :: Parser Char
symbol = oneOf "!$%&|*+.-/:<=>?@^_~"

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


parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (escapeChar <|> noneOf "\"")
                 char '"'
                 return $ String x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ Atom atom

parsePound :: Parser LispVal
parsePound = do char '#'
                x <- anyChar
                case x of
                  't' -> return $ Bool True
                  'f' -> return $ Bool False
                  'd' -> parseNumber readDec digit
                  'b' -> parseNumber readBin (oneOf "01")
                  'o' -> parseNumber readOct octDigit
                  'x' -> parseNumber readHex hexDigit

--I'll let Haskell infer for now
parseNumber f d = do num <- many1 d
                     return $ Number (fst (head (f num))) -- We have a lot of unused power here, which will be utilized when I solve exercise 6.

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber readDec digit
        <|> parsePound


-- Parsing logic
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> "No match: " ++ show err
                   Right val -> show val

-- Main Loop
main :: IO ()
main = do
  args <- getArgs
  putStrLn (readExpr (args !! 0))