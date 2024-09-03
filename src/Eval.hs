module Eval(eval) where
import Parser (LispVal (..))

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval val@(Char _) = val
eval val@(Float _) = val
eval val@(Rational _) = val
eval val@(Complex _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args
eval others = others

apply :: String -> [LispVal] -> LispVal
apply f args = maybe (Bool False) ($ args) $ lookup f primitives

primitives :: [(String, [LispVal] -> LispVal)]
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

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop f args = Number $ foldl1 f $ unpackList args

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp f [x] = f x
unaryOp _ _ = Bool False

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

unpackList :: [LispVal] -> [Integer]
unpackList = map unpackNumber

unpackNumber :: LispVal -> Integer
unpackNumber (Number n) = n
unpackNumber _ = 0