module Parser ( parseScheme ) where
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric(readInt, readDec, readOct, readHex, readFloat)
import Char (digitToInt)
import List (sort)
import Monad (liftM)
import qualified Data.Map as Map
import Datatypes

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
symbol = initialSymbol <|> oneOf "+-.@"

-- '+', '-', and '.' aren't allowed to be the first character of an atom
initialSymbol :: Parser Char
initialSymbol = oneOf "!$%&*/:<=>?^_~"

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
parseAtom = do first <- letter <|> initialSymbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ Atom atom

-- These are special atoms defined in R5RS spec that begin with "unallowed" symbols
parsePeculiarAtom :: Parser LispVal
parsePeculiarAtom = liftM Atom $ try (string "...") <|> string "+" <|> string "-"

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
        <|> parsePeculiarAtom
        <|> parseString
        <|> parseDec '?'
        <|> parsePound
        <|> parseQuoted
        <|> parseQuasiQuoted
        <|> parseUnquoteSplicing
        <|> parseUnquote
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
  char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

parseUnquote :: Parser LispVal
parseUnquote = do
  try (char ',')
  x <- parseExpr
  return $ List [Atom "unquote", x]

parseUnquoteSplicing :: Parser LispVal
parseUnquoteSplicing = do
  try (string ",@")
  x <- parseExpr
  return $ List [Atom "unquote-splicing", x]

-- Parsing logic
parseScheme :: String -> String
parseScheme input = case parse parseExpr "lisp" input of
                   Left err -> "No match: " ++ show err
                   Right val -> show val
