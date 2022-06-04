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
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Foldable
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Generics
import OurScheme.AST
import OurScheme.Eval
import OurScheme.Parser
import Text.Megaparsec

data Env = Env
  { envBinds :: IORef [(Symbol, SExp)]
  }
  deriving (Generic)

blankEnv :: IO Env
blankEnv = Env <$> newIORef []

newtype EnvT m a = EnvT (ReaderT Env (ExceptT T.Text m) a)
  deriving (Functor, Applicative, Monad, MonadIO)
  deriving (MonadError T.Text)
  deriving
    (HasState "binds" [(Symbol, SExp)], HasSource "binds" [(Symbol, SExp)], HasSink "binds" [(Symbol, SExp)])
    via (ReaderIORef (Rename "envBinds" (Field "envBinds" () (MonadReader (ReaderT Env (ExceptT T.Text m))))))
  deriving
    (HasReader "binds" [(Symbol, SExp)])
    via (ReadStatePure (ReaderIORef (Rename "envBinds" (Field "envBinds" () (MonadReader (ReaderT Env (ExceptT T.Text m)))))))

runEnvWith :: EnvT m a -> Env -> m (Either T.Text a)
runEnvWith (EnvT m) env = runExceptT $ runReaderT m env

repl :: IO (Either T.Text ())
repl = do
  env <- blankEnv
  flip runEnvWith env . forever $ do
    liftIO $ T.putStr "> "
    result <- incrementalParsing
    case result of
      Left err -> do
        liftIO $ T.putStrLn $ "Error: " <> T.pack (errorBundlePretty err)
        liftIO $ print $ isEndOfInputError err
      Right sexp -> do
        ret <- tryError $ topLevelNormalize sexp
        liftIO $ T.putStrLn $ T.pack (show ret)
  where
    incrementalParsing = incrementalParsing' ""

    incrementalParsing' prevCmd = do
      cmd <- (prevCmd <>) <$> liftIO T.getLine
      case runParser (space *> pSExp <* eof) "repl" cmd of
        Left err ->
          if isEndOfInputError err
            then incrementalParsing' (cmd <> "\n")
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
