module Error (LispError (..), ThrowsError, extractValue, trapError) where

import Control.Monad.Except
import Parser
import Text.ParserCombinators.Parsec hiding (spaces)

data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String

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
