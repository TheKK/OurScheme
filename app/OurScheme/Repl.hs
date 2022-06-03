{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module OurScheme.Repl
  ( repl,
  )
where

import Capability.State.Internal.Strategies (MonadState (MonadState))
import Control.Exception
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Data.Foldable
import Data.Function
import qualified Data.Text as T
import qualified Data.Text.IO as T
import OurScheme.Eval
import OurScheme.Parser
import Text.Megaparsec

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
      case runParser (pSExp <* eof) "repl" cmd of
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
