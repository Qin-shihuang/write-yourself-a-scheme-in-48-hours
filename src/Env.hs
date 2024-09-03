module Env (Env, IOThrowsError, nullEnv, liftThrows, runIOThrows, isBound, getVar, setVar, defineVar) where

import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Functor
import Data.IORef
import qualified Data.Map as Map
import Error (LispError (UnboundVar), ThrowsError, extractValue, trapError)
import Parser (LispVal)

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