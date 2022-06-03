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
    cmd <- liftIO T.getLine
    unless (T.all (== ' ') cmd) $ do
      case runParser pSExp "repl" cmd of
        Left err -> liftIO $ T.putStrLn $ "Error: " <> T.pack (errorBundlePretty err)
        Right sexp -> do
          ret <- tryError $ topLevelNormalize sexp
          liftIO $ T.putStrLn $ T.pack (show ret)

tryError :: (MonadError e m) => m a -> m (Either e a)
tryError m = fmap Right m `catchError` (pure . Left)
