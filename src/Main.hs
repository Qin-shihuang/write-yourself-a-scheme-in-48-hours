module Main (main, readExpr) where

import System.Environment
import Parser
import Eval
import Text.ParserCombinators.Parsec hiding (spaces)

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> String $ "No match: " ++ show err
  Right val -> val

main :: IO ()
main = do
  getArgs >>= print . eval . readExpr . head
