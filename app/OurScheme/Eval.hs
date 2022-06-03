{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module OurScheme.Eval where

import Capability.Reader
import Capability.Sink
import Capability.Source
import Capability.State
import Capability.Writer
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader (ReaderT (..), runReader)
import Data.Functor
import Data.IORef
import Data.List.NonEmpty
import qualified Data.Text as T
import GHC.Generics
import OurScheme.AST

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

data Result = RInt Int | RText T.Text

instance Show Result where
  show (RInt l) = show l
  show (RText l) = show l

eval :: SExp -> IO (Either T.Text SExp)
eval sexp = blankEnv >>= runEnvWith (normalize sexp)

topLevelNormalize :: (MonadError T.Text m, HasState "binds" [(Symbol, SExp)] m, HasReader "binds" [(Symbol, SExp)] m) => SExp -> m SExp
topLevelNormalize exp@(SDefSym sym sexp) = modify' @"binds" ((sym, sexp) :) >> pure exp
topLevelNormalize other = normalize other

normalize :: (MonadError T.Text m, HasReader "binds" [(Symbol, SExp)] m) => SExp -> m SExp
normalize exp@(SLit _) = pure exp
normalize (SSym sym) = do
  v <- reader @"binds" $ lookup sym
  case v of
    Just v' -> normalize v'
    Nothing -> throwError $ "no such symbol: " <> (T.pack . show) sym
normalize (SLet binds bodies) = foldr singleLet bodiesRun binds
  where
    singleLet bind bodies' = do
      nb <- mapM normalize bind
      local @"binds" (nb :) bodies'
    bodiesRun = foldr1 (>>) $ fmap normalize $ bodies
normalize (SDefSym sym sexp) = throwError "can not using define at here"
