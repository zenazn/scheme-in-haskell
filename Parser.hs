module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric(readInt, readDec, readOct, readHex, readFloat)
import Char (digitToInt)
import List (sort)
import qualified Data.Map as Map

-- Datatypes
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal -- Apparently a valid datatype in Scheme
             | Number Integer
             | Float Float
             | String String
             | Bool Bool
             | Char Char
               deriving (Show)

-- Supporting Functions

readBin :: (Integral a) => ReadS a
readBin = readInt 2 (\c -> c == '0' || c == '1') digitToInt

--Supporting Definitions

charIDs = Map.fromList([("space",' '),("newline",'\n'),("tab",'\t')])
numChars = "sdflSDFL."

--Shortcut Functions

parseDec = parseNumber readDec (digit, oneOf ("012346789" ++ numChars))
parseBin = parseNumber readBin (oneOf ("01"), oneOf ("01" ++ numChars))
parseOct = parseNumber readOct (octDigit, oneOf ("01234567" ++ numChars))
parseHex = parseNumber readHex (hexDigit, oneOf ("0123456789ABCDEFabcdef" ++ numChars))


-- Char
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
parsePound = do
  char '#'
  x <- anyChar
  if (x == '\\') then
      parseChar
      else do
          y <- (do {char '#'; anyChar} <|> return '#')
          case (sort (x:y:[])) of
            "#t" -> return $ Bool True
            "#f" -> return $ Bool False
            "#d" -> parseDec '?'
            "#b" -> parseBin '?'
            "#o" -> parseOct '?'
            "#x" -> parseHex '?'
            "di" -> parseDec 'i'
            "bi" -> parseBin 'i'
            "io" -> parseOct 'i'
            "ix" -> parseHex 'i'
            "de" -> parseDec 'e'
            "be" -> parseBin 'e'
            "eo" -> parseOct 'e'
            "ex" -> parseHex 'e'

parseChar :: Parser LispVal
parseChar = do ident <- many (noneOf " ")
               case length ident of
                 0 -> return $ Char ' ' -- Will eventually need to be more robust.
                 1 -> return $ Char (head ident)
                 _ -> return $ Char ((\(Just x) -> x) (Map.lookup ident charIDs))

--I'll let Haskell infer for now
parseNumber f d p = do num <- many1 (fst d)
                       return $ Number (fst (head (f num))) -- We have a lot of unused power here, which will be utilized when I solve exercise 6.

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseDec '?'
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