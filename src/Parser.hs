module Parser (LispVal (..), parseExpr, showVal, spaces, unwordsList) where

import Data.Array (Array, listArray, elems)
import Data.Complex (Complex ((:+)))
import Data.Ratio ((%))
import Numeric (readBin, readHex, readOct)
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Vector (Array Int LispVal)
  | Bool Bool
  | Char Char
  | Float Double
  | Rational Rational
  | Complex (Complex Double)
  deriving (Eq)

symbol :: Parser Char
symbol = oneOf "!#%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseExpr :: Parser LispVal
parseExpr =
        try parseBool
    <|> parseVector
    <|> try parseComplex
    <|> try parseFloat
    <|> try parseRational
    <|> parseNumber
    <|> parseChar
    <|> parseAtom
    <|> parseString
    <|> parseQuoted
    <|> parseQuasiQuoted
    <|> parseUnQuote
    <|> do
      _ <- char '('
      x <- try parseList <|> parseDottedList
      _ <- char ')'
      return x

parseBool :: Parser LispVal
parseBool = do
  _ <- char '#'
  (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseFloat :: Parser LispVal
parseFloat = do
  x <- many1 digit
  _ <- char '.'
  y <- many1 digit
  return $ Float $ read $ x ++ "." ++ y

parseComplex :: Parser LispVal
parseComplex = do
  x <- try parseFloat <|> parseNumber
  _ <- char '+'
  y <- try parseFloat <|> parseNumber
  _ <- char 'i'
  return $ Complex (toDouble x :+ toDouble y)
  where
    toDouble (Float f) = f
    toDouble (Number n) = fromIntegral n
    toDouble _ = error "Not gonna happen"

parseRational :: Parser LispVal
parseRational = do
  x <- many1 digit
  _ <- char '/'
  y <- many1 digit
  return $ Rational $ read x % read y

parseNumber :: Parser LispVal
parseNumber =
        parseRawDecimal
    <|> parseBinary
    <|> parseOctal
    <|> parseDecimal
    <|> parseHexadecimal

parseRawDecimal :: Parser LispVal
parseRawDecimal = Number . read <$> many1 digit

parseBinary :: Parser LispVal
parseBinary = do
  _ <- string "#b"
  d <- many1 (oneOf "01")
  return $ Number $ fst $ head $ readBin d

parseOctal :: Parser LispVal
parseOctal = do
  _ <- string "#o"
  d <- many1 octDigit
  return $ Number $ fst $ head $ readOct d

parseDecimal :: Parser LispVal
parseDecimal = do
  _ <- string "#d"
  d <- many1 digit
  return $ Number $ read d

parseHexadecimal :: Parser LispVal
parseHexadecimal = do
  _ <- string "#x"
  d <- many1 hexDigit
  return $ Number $ fst $ head $ readHex d

parseChar :: Parser LispVal
parseChar = do
  _ <- string "#\\"
  c <- many1 letter
  return $ Char $ case c of
    "space" -> ' '
    "newline" -> '\n'
    [x] -> x
    _ -> error "Invalid character"

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  others <- many (letter <|> digit <|> symbol)
  return $ Atom (first : others)

parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  x <-
    many
      ( noneOf "\\\""
          <|> ( char '\\' >> oneOf "nrt\\\"" >>= \c -> return $ case c of
                  'n' -> '\n'
                  'r' -> '\r'
                  't' -> '\t'
                  _ -> c
              )
      )
  _ <- char '"'
  return $ String x

parseVector :: Parser LispVal
parseVector = do
  _ <- string "#("
  x <- sepBy parseExpr spaces
  _ <- char ')'
  return $ Vector (listArray (0, length x - 1) x)

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  listHead <- endBy parseExpr spaces
  listTail <- char '.' >> spaces >> parseExpr
  return $ DottedList listHead listTail

parseQuoted :: Parser LispVal
parseQuoted = char '\'' >> parseExpr >>= \x -> return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = char '`' >> parseExpr >>= \x -> return $ List [Atom "quasiquote", x]

parseUnQuote :: Parser LispVal
parseUnQuote = char ',' >> parseExpr >>= \x -> return $ List [Atom "unquote", x]

instance Show LispVal where
  show = showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (Vector arr) = "(" ++ unwordsList (elems arr) ++ ")"
showVal (DottedList listHead listTail) = "(" ++ unwordsList listHead ++ "." ++ showVal listTail ++ ")"
showVal (Char c) = "#\\" ++ [c]
showVal (Float f) = show f
showVal (Rational r) = show r
showVal (Complex c) = show c

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal