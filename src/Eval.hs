{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Eval (eval, primitiveBindings) where

import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Functor ((<&>))
import Data.Maybe (isNothing)
import Parser (Env, IOThrowsError, LispError (..), LispVal (..), ThrowsError, bindVars, defineVar, getVar, liftThrows, nullEnv, setVar, showVal, parseExpr)
import Prelude hiding (pred)
import Text.ParserCombinators.Parsec hiding (string)
import System.IO

eval :: Env -> LispVal -> IOThrowsError LispVal
eval _ val@(String _) = return val
eval _ val@(Number _) = return val
eval _ val@(Bool _) = return val
eval _ val@(Char _) = return val
eval _ val@(Float _) = return val
eval _ val@(Rational _) = return val
eval _ val@(Complex _) = return val
eval env (Atom var) = getVar env var
eval _ (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) = do
  result <- eval env pred
  case result of
    Bool False -> eval env alt
    Bool True -> eval env conseq
    _ -> throwError $ TypeMismatch "boolean" result
eval _ (List [Atom "cond"]) = throwError $ BadSpecialForm "No true clause in cond expression: " (List [Atom "cond"])
eval env (List (Atom "cond" : (List [test, expr]) : clauses)) =
  if test == Atom "else"
    then
      if null clauses
        then eval env expr
        else throwError $ BadSpecialForm "else clause isn't last: " (List [test, expr])
    else do
      result <- eval env test
      case result of
        Bool True -> eval env expr
        Bool False -> eval env (List (Atom "cond" : clauses))
        pred -> throwError $ TypeMismatch "boolean" pred
eval env form@(List (Atom "case" : key : clauses)) =
  if null clauses
    then
      throwError $ BadSpecialForm "No true clause in case expression: " form
    else case head clauses of
      List (Atom "else" : exprs) -> mapM (eval env) exprs <&> last
      List (List datums : exprs) -> do
        result <- eval env key
        equality <- mapM (\x -> liftThrows (eqv [result, x])) datums
        if Bool True `elem` equality
          then mapM (eval env) exprs <&> last
          else eval env $ List (Atom "case" : key : tail clauses)
      _ -> throwError $ BadSpecialForm "Ill-formed case expression: " form
eval env (List [Atom "set!", Atom var, form]) =
  eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
  eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
  makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
  makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
  makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
  makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
  makeVarArgs varargs env [] body
eval env (List [Atom "load", String filename]) = 
     load filename >>= fmap last . mapM (eval env)
eval env (List (function : args)) = do
  func <- eval env function
  argVals <- mapM (eval env) args
  apply func argVals
eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
  if num params /= num args && isNothing varargs
    then throwError $ NumArgs (num params) args
    else liftIO (bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
  where
    remainingArgs = drop (length params) args
    num = toInteger . length
    evalBody env = last <$> mapM (eval env) body
    bindVarArgs arg env = case arg of
      Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
      Nothing -> return env
apply (IOFunc func) args = func args
apply _ _ = throwError $ Default "Unknown error in apply"

makeFunc :: (Monad m) => Maybe String -> Env -> [LispVal] -> [LispVal] -> m LispVal
makeFunc varargs env params body = return $ Func (map showVal params) varargs body env

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> ExceptT LispError IO LispVal
makeNormalFunc = makeFunc Nothing

makeVarArgs :: LispVal -> Env -> [LispVal] -> [LispVal] -> ExceptT LispError IO LispVal
makeVarArgs = makeFunc . Just . showVal

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= flip bindVars (map (makeFunc IOFunc) ioPrimitives
                                               ++ map (makeFunc PrimitiveFunc) primitives)
     where makeFunc constructor (var, func) = (var, constructor func)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
  [ ("+", numericBinop (+)),
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
    ("string->symbol", unaryOp string2symbol),
    ("=", numBoolBinop (==)),
    ("<", numBoolBinop (<)),
    (">", numBoolBinop (>)),
    ("/=", numBoolBinop (/=)),
    (">=", numBoolBinop (>=)),
    ("<=", numBoolBinop (<=)),
    ("&&", boolBoolBinop (&&)),
    ("||", boolBoolBinop (||)),
    ("string=?", strBoolBinop (==)),
    ("string<?", strBoolBinop (<)),
    ("string>?", strBoolBinop (>)),
    ("string<=?", strBoolBinop (<=)),
    ("string>=?", strBoolBinop (>=)),
    ("car", car),
    ("cdr", cdr),
    ("cons", cons),
    ("eq?", eqv),
    ("eqv?", eqv),
    ("equal?", equal),
    ("make-string", makeString),
    ("string", string),
    ("string-length", stringLength),
    ("string-ref", stringRef),
    ("string-set!", stringSet)
  ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ [] = throwError $ NumArgs 2 []
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop f args = Number . foldl1 f <$> mapM unpackNumber args

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp _ [] = throwError $ NumArgs 1 []
unaryOp f [v] = return $ f v
unaryOp _ many = throwError $ NumArgs 1 many

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args =
  if length args /= 2
    then throwError $ NumArgs 2 args
    else do
      left <- unpacker $ head args
      right <- unpacker $ args !! 1
      return $ Bool $ left `op` right

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop = boolBinop unpackNumber

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackString

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

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
unpackNumber (String n) =
  let parsed = reads n
   in if null parsed
        then throwError $ TypeMismatch "number" $ String n
        else return $ fst $ head parsed
unpackNumber (List [n]) = unpackNumber n
unpackNumber notNum = throwError $ TypeMismatch "number" notNum

unpackString :: LispVal -> ThrowsError String
unpackString (String s) = return s
unpackString (Number s) = return $ show s
unpackString (Bool s) = return $ show s
unpackString notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)] = return x
car [DottedList (x : _) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool arg1, Bool arg2] = return $ Bool $ arg1 == arg2
eqv [Number arg1, Number arg2] = return $ Bool $ arg1 == arg2
eqv [String arg1, String arg2] = return $ Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2] = return $ Bool $ arg1 == arg2
eqv [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List arg1, List arg2] = eqvList eqv [List arg1, List arg2]
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

eqvList :: ([LispVal] -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
eqvList eqvFunc [List arg1, List arg2] =
  return $
    Bool $
      length arg1 == length arg2
        && all eqvPair (zip arg1 arg2)
  where
    eqvPair (x1, x2) = case eqvFunc [x1, x2] of
      Left _ -> False
      Right (Bool val) -> val
      _ -> False
eqvList _ _ = throwError $ Default "Unexpected error in eqvList"

data Unpacker = forall a. (Eq a) => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  do
    unpacked1 <- unpacker arg1
    unpacked2 <- unpacker arg2
    return $ unpacked1 == unpacked2
    `catchError` const (return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [l1@(List _), l2@(List _)] = eqvList equal [l1, l2]
equal [arg1, arg2] = do
  primitiveEquals <-
    or
      <$> mapM
        (unpackEquals arg1 arg2)
        [AnyUnpacker unpackNumber, AnyUnpacker unpackString, AnyUnpacker unpackBool]
  eqvEquals <- eqv [arg1, arg2]
  case (primitiveEquals, eqvEquals) of
    (True, Bool True) -> return $ Bool True
    _ -> return $ Bool False
equal badArgList = throwError $ NumArgs 2 badArgList

makeString :: [LispVal] -> ThrowsError LispVal
makeString [] = return $ String ""
makeString (Number n : _) | n < 0 = throwError $ Default "Negative length"
makeString [Number n] = return $ String $ replicate (fromIntegral n) ' '
makeString [Number n, Char c] = return $ String $ replicate (fromIntegral n) c
makeString [badArg] = throwError $ TypeMismatch "number or char" badArg
makeString badArgList = throwError $ NumArgs 2 badArgList

string :: [LispVal] -> ThrowsError LispVal
string (Char c : cs) =
  let rest = string cs
   in case rest of
        Right (String s) -> return $ String $ c : s
        Left err -> Left err
        _ -> Left $ Default "Unexpected error in string"
string [] = return $ String ""
string badArgList = throwError $ TypeMismatch "char" $ List badArgList

stringLength :: [LispVal] -> ThrowsError LispVal
stringLength [] = throwError $ NumArgs 1 []
stringLength [String s] = return $ Number $ fromIntegral $ length s
stringLength [badArg] = throwError $ TypeMismatch "string" badArg
stringLength badArgList = throwError $ NumArgs 1 badArgList

stringRef :: [LispVal] -> ThrowsError LispVal
stringRef [] = throwError $ NumArgs 2 []
stringRef [String s, Number n] | fromIntegral n < length s = return $ Char $ s !! fromIntegral n
stringRef [String _, Number _] = throwError $ Default "Out of bounds"
stringRef [badArg] = throwError $ TypeMismatch "string and number" badArg
stringRef badArgList = throwError $ NumArgs 2 badArgList

-- TODO
stringSet :: [LispVal] -> ThrowsError LispVal
stringSet = undefined

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err  -> throwError $ Parser err
    Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr
readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces)

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args
applyProc args              = throwError $ NumArgs 2 args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = fmap Port $ liftIO $ openFile filename mode
makePort _ [badArg] = throwError $ TypeMismatch "string" badArg
makePort _ badArgList = throwError $ NumArgs 1 badArgList

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> return (Bool True)
closePort _           = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = liftIO (hGetLine port) >>= liftThrows . readExpr
readProc [badArg]    = throwError $ TypeMismatch "port" badArg
readProc badArgList  = throwError $ NumArgs 1 badArgList

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> return (Bool True)
writeProc [_, badArg]      = throwError $ TypeMismatch "port" badArg
writeProc badArgList       = throwError $ NumArgs 2 badArgList

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = fmap String $ liftIO $ readFile filename
readContents [badArg]          = throwError $ TypeMismatch "string" badArg
readContents badArgList        = throwError $ NumArgs 1 badArgList

load :: String -> IOThrowsError [LispVal]
load filename = liftIO (readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = List <$> load filename
readAll [badArg]          = throwError $ TypeMismatch "string" badArg
readAll badArgList        = throwError $ NumArgs 1 badArgList