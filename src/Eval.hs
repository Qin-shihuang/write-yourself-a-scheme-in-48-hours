module Eval(eval) where
import Parser (LispVal (..))
import Error (ThrowsError, LispError (..))
import Control.Monad.Except

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval val@(Char _) = return val
eval val@(Float _) = return val
eval val@(Rational _) = return val
eval val@(Complex _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply f args = maybe (throwError $ NotFunction "Unrecognized primitive function args" f) ($ args) $ lookup f primitives

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [
    ("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop div),
    ("mod", numericBinop mod),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem),
    ("symbol?", unaryOp isSymbol),
    ("char?", unaryOp isChar),
    ("string?", unaryOp isString),
    ("number?", unaryOp isNumber),
    ("bool?", unaryOp isBool),
    ("list?", unaryOp isList),
    ("vector?", unaryOp isVector),
    ("dotted-list?", unaryOp isDottedList),
    ("symbol->string", unaryOp symbol2string),
    ("string->symbol", unaryOp string2symbol)
    ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ [] = throwError $ NumArgs 2 []
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop f args = Number . foldl1 f <$> mapM unpackNumber args

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp _ [] = throwError $ NumArgs 1 []
unaryOp f [v] = return $ f v
unaryOp _ many = throwError $ NumArgs 1 many


isSymbol, isChar, isString, isNumber, isBool, isList, isVector, isDottedList :: LispVal -> LispVal
isSymbol (Atom _) = Bool True
isSymbol _ = Bool False

isChar (Char _) = Bool True
isChar _ = Bool False

isString (String _) = Bool True
isString _ = Bool False

isNumber (Number _) = Bool True
isNumber _ = Bool False

isBool (Bool _) = Bool True
isBool _ = Bool False

isList (List _) = Bool True
isList _ = Bool False

isVector (Vector _) = Bool True
isVector _ = Bool False

isDottedList (DottedList _ _) = Bool True
isDottedList _ = Bool False

symbol2string, string2symbol :: LispVal -> LispVal
symbol2string (Atom s) = String s
symbol2string _ = String ""

string2symbol (String s) = Atom s
string2symbol _ = Atom ""

unpackNumber :: LispVal -> ThrowsError Integer
unpackNumber (Number n) = return n
unpackNumber (String n) = let parsed = reads n in
  if null parsed
    then throwError $ TypeMismatch "number" $ String n
    else return $ fst $ head parsed
unpackNumber (List [n]) = unpackNumber n
unpackNumber notNum = throwError $ TypeMismatch "number" notNum