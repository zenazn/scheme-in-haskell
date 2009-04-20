module Parser ( parseScheme ) where
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric(readInt, readDec, readOct, readHex, readFloat)
import Ratio
import Complex
import Char (digitToInt)
import List (sort)
import Monad (liftM)
import qualified Data.Map as Map
import Datatypes

-- Supporting Functions

readBin :: (Integral a) => ReadS a
readBin = readInt 2 (\c -> c == '0' || c == '1') digitToInt

fh :: [(a, b)] -> a
fh x = fst (head (x))

readBinFloat = readFloat -- Placeholders.
readOctFloat = readFloat
readHexFloat = readFloat

--Supporting Definitions

charIDs = Map.fromList([("space",' '),("newline",'\n'),("tab",'\t')])
expChars = "sdflSDFL"

--Shortcut Functions
 
parseDec = parseNumber 10.0 (readDec, readFloat) digit
parseBin = parseNumber 2.0 (readBin, readBinFloat) (oneOf ("01"))
parseOct = parseNumber 8.0 (readOct, readOctFloat) octDigit
parseHex = parseNumber 16.0 (readHex, readHexFloat) hexDigit


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
            "#e" -> parseDec 'e'
            "#i" -> parseDec 'i'
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
parseNumber b (f,g) d p' = do 
  predec <- many1 d
  dec <- (do {char '.'; many d} <|> return "!")
  expchar <- (do {oneOf expChars} <|> return '!')
  exp <- (if expchar /= '!' then many d else return "0")
  let p = (if p' == '?' then if (dec /= "!" || expchar /= '!') then 'i' else 'e' else p')
  let r = if (dec == "!" && expchar == '!') then (Number ((fh (f ("0" ++ predec))) * (round b) ^ (fh (f exp))))
              else if p == 'i' then (Float (((fh (g ("0" ++ predec ++ "." ++ dec ++ "0" )))) * b ^ (fh (f exp))))
                       else (Rational ((((fh (f ("0" ++ predec))) % 1) + ((fh (f ("0" ++ dec))) % (round b) ^ (length dec)) * (((round b) ^ (fh (f (exp)))) % 1))))
  return r

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
        <|> parseAnyList

parseAnyList :: Parser LispVal
parseAnyList = do p <- char '('
                  many space
                  x <- sepEndBy parseExpr spaces
                  c <- char '.' <|> char ')'
                  if c == '.' then do 
                                y <- spaces >> parseExpr
                                many space
                                char ')'
                                return $ DottedList x y
                    else do return $ List x

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
parseScheme :: String -> LispVal
parseScheme input = case parse parseExpr "lisp" input of
                   Left err -> String ("No match: " ++ show err)
                   Right val -> val