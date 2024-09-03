module Main (main, readExpr) where

import System.Environment
import Parser
import Eval
import Text.ParserCombinators.Parsec hiding (spaces)
import Error (LispError(..), ThrowsError, extractValue, trapError)
import Control.Monad.Except

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ extractValue $ trapError $ fmap show $ readExpr (head args) >>= eval