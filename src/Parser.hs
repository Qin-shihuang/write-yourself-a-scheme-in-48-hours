{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Parser (LispVal (..), parseExpr, showVal, spaces, unwordsList, LispError (..), ThrowsError, trapError, extractValue, Env, liftThrows, nullEnv, runIOThrows, IOThrowsError, getVar, setVar, defineVar, bindVars) where

import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Array (Array, elems, listArray)
import Data.Complex (Complex ((:+)))
import Data.Functor
import Data.IORef
import qualified Data.Map as Map
import Data.Ratio ((%))
import Numeric (readBin, readHex, readOct)
import Text.ParserCombinators.Parsec
  ( ParseError,
    Parser,
    char,
    digit,
    endBy,
    hexDigit,
    letter,
    many,
    many1,
    noneOf,
    octDigit,
    oneOf,
    sepBy,
    skipMany1,
    space,
    string,
    try,
    (<|>),
  )
import Control.Monad (foldM)
import System.IO (Handle)

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
  | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
  | Func {params :: [String], vararg :: Maybe String, body :: [LispVal], closure :: Env}
  | IOFunc ([LispVal] -> IOThrowsError LispVal)
  | Port Handle
data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String

instance Eq LispVal where
  (==) (Atom a) (Atom b) = a == b
  (==) (List a) (List b) = a == b
  (==) (DottedList a b) (DottedList c d) = a == c && b == d
  (==) (Number a) (Number b) = a == b
  (==) (String a) (String b) = a == b
  (==) (Vector a) (Vector b) = a == b
  (==) (Bool a) (Bool b) = a == b
  (==) (Char a) (Char b) = a == b
  (==) (Float a) (Float b) = a == b
  (==) (Rational a) (Rational b) = a == b
  (==) (Complex a) (Complex b) = a == b
  (==) _ _ = False

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
showVal (DottedList listHead listTail) = "(" ++ unwordsList listHead ++ " . " ++ showVal listTail ++ ")"
showVal (Char c) = "#\\" ++ [c]
showVal (Float f) = show f
showVal (Rational r) = show r
showVal (Complex c) = show c
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = _, closure = _}) =
  "(lambda (" ++ unwords (map show args) ++ (case varargs of Just arg -> " . " ++ arg; Nothing -> "") ++ ") ...)"
showVal (IOFunc _) = "<IO primitive>"
showVal (Port _) = "<IO port>"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispError where
  show = showError

showError :: LispError -> String
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (Default message) = message

instance Eq LispError where
  (==) = eqError

eqError :: LispError -> LispError -> Bool
eqError (NumArgs a1 b1) (NumArgs a2 b2) = a1 == a2 && b1 == b2
eqError (TypeMismatch a1 b1) (TypeMismatch a2 b2) = a1 == a2 && b1 == b2
eqError (Parser a1) (Parser a2) = a1 == a2
eqError (BadSpecialForm a1 b1) (BadSpecialForm a2 b2) = a1 == a2 && b1 == b2
eqError (NotFunction a1 b1) (NotFunction a2 b2) = a1 == a2 && b1 == b2
eqError (UnboundVar a1 b1) (UnboundVar a2 b2) = a1 == a2 && b1 == b2
eqError (Default a1) (Default a2) = a1 == a2
eqError _ _ = False

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

type ThrowsError = Either LispError

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue _ = undefined

type Env = IORef (Map.Map String (IORef LispVal))

nullEnv :: IO Env
nullEnv = newIORef Map.empty

type IOThrowsError = ExceptT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) <&> extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef <&> Map.member var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Getting an unbound variable" var) (liftIO . readIORef) (Map.lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Setting an unbound variable" var) (liftIO . flip writeIORef value) (Map.lookup var env)
  return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value $> value
    else liftIO $ do
      valueRef <- newIORef value
      modifyIORef envRef $ Map.insert var valueRef
      return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = do
  env <- readIORef envRef
  newEnv <- foldM addBinding env bindings
  newIORef newEnv
  where
    addBinding :: Map.Map String (IORef LispVal) -> (String, LispVal) -> IO (Map.Map String (IORef LispVal))
    addBinding env (var, value) = do
      valueRef <- newIORef value
      return $ Map.insert var valueRef env
