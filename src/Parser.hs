module Parser (LispVal(..), parseExpr) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric (readBin, readOct, readHex)
import Data.Complex (Complex ((:+)))
import Data.Ratio ((%))

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  | Char Char
  | Float Double
  | Rational Rational
  | Complex (Complex Double)
  deriving (Show)

parseExpr :: Parser LispVal
parseExpr = try parseBool
        <|> try parseComplex
        <|> try parseFloat
        <|> try parseRational
        <|> try parseNumber
        <|> try parseChar
        <|> parseAtom
        <|> parseString

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
    where toDouble (Float f) = f
          toDouble (Number n) = fromIntegral n
          toDouble _ = error "Not gonna happen"

parseRational :: Parser LispVal
parseRational = do
    x <- many1 digit
    _ <- char '/'
    y <- many1 digit
    return $ Rational $ read x % read y

parseNumber :: Parser LispVal
parseNumber = parseRawDecimal
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
  atom <- many (letter <|> digit <|> symbol)
  return $ Atom atom

parseString :: Parser LispVal
parseString = do
    _ <- char '"'
    x <- many (noneOf "\\\"" <|> (char '\\' >> oneOf "nrt\\\"" >>= \c -> return $ case c of
        'n' -> '\n'
        'r' -> '\r'
        't' -> '\t'
        _ -> c))
    _ <- char '"'
    return $ String x


symbol :: Parser Char
symbol = oneOf "!#%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space
