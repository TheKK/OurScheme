{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module OurScheme.Repl
  ( repl,
  )
where

import Capability.Reader
import Capability.Sink
import Capability.Source
import Capability.State
import Control.Exception (AsyncException (UserInterrupt), throwIO)
import Control.Monad
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow (throwM))
import Control.Monad.Error.Class
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Trans (lift)
import Data.Foldable
import Data.IORef
import qualified Data.Text as T
import Data.Void
import GHC.Generics
import OurScheme.AST
import OurScheme.Eval
import OurScheme.Parser
import System.Console.Haskeline
import Text.Megaparsec

data Env = Env
  { envBinds :: IORef [(Symbol, SExp)]
  }
  deriving (Generic)

blankEnv :: IO Env
blankEnv = Env <$> newIORef []

newtype EnvT m a = EnvT (ReaderT Env (ExceptT T.Text m) a)
  deriving (Functor, Applicative, Monad, MonadIO)
  deriving (MonadThrow, MonadCatch, MonadMask)
  deriving (MonadError T.Text)
  deriving
    (HasState "binds" [(Symbol, SExp)], HasSource "binds" [(Symbol, SExp)], HasSink "binds" [(Symbol, SExp)])
    via (ReaderIORef (Rename "envBinds" (Field "envBinds" () (MonadReader (ReaderT Env (ExceptT T.Text m))))))
  deriving
    (HasReader "binds" [(Symbol, SExp)])
    via (ReadStatePure (ReaderIORef (Rename "envBinds" (Field "envBinds" () (MonadReader (ReaderT Env (ExceptT T.Text m)))))))

runEnvWith :: (MonadIO m, MonadMask m) => EnvT m a -> Env -> m (Either T.Text a)
runEnvWith (EnvT m) env = runExceptT $ runReaderT m env

getInputLineText :: (MonadIO m, MonadMask m) => String -> InputT m T.Text
getInputLineText prompt = do
  cmd <- fmap T.pack <$> getInputLine prompt
  case cmd of
    Nothing -> throwM UserInterrupt
    Just cmd' -> pure cmd'

outputStrLnText :: (MonadIO m) => T.Text -> InputT m ()
outputStrLnText = outputStrLn . T.unpack

repl :: IO (Either T.Text ())
repl = do
  env <- blankEnv
  (flip runEnvWith env . runInputT defaultSettings . forever) $ do
    result <- incrementalParsing
    case result of
      Left err -> do
        outputStrLnText $ "Error: " <> T.pack (errorBundlePretty err)
      Right sexp -> do
        ret <- lift $ tryError $ topLevelNormalize sexp
        outputStrLnText $ T.pack (show ret)
  where
    incrementalParsing = incrementalParsing' "> " ""
    incrementalParsing' prompt prevCmd = do
      cmd <- (prevCmd <>) <$> getInputLineText prompt
      case runParser (space *> pSExp <* eof) "repl" cmd of
        Left err ->
          if isEndOfInputError err
            then incrementalParsing' "" (cmd <> "\n")
            else pure $ Left err
        Right sexp -> pure $ Right sexp

tryError :: (MonadError e m) => m a -> m (Either e a)
tryError m = fmap Right m `catchError` (pure . Left)

isEndOfInputError :: (Eq (Token t)) => ParseErrorBundle t e -> Bool
isEndOfInputError (ParseErrorBundle parseErrors _) =
  or $
    fmap
      ( \case
          (TrivialError _ unexpected _) -> EndOfInput `elem` toList unexpected
          (FancyError _ _) -> False
      )
      $ parseErrors
