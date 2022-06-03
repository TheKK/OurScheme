{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
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
import Data.List.NonEmpty (NonEmpty)
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
topLevelNormalize exp@(SDefSym sym sexp) = normalize sexp >>= \sexp' -> modify' @"binds" ((sym, sexp') :) >> pure sexp'
topLevelNormalize other = normalize other

normalize :: (MonadError T.Text m, HasReader "binds" [(Symbol, SExp)] m) => SExp -> m SExp
normalize exp@(SLit _) = pure exp
normalize (SSym sym) = do
  v <- reader @"binds" $ lookup sym
  case v of
    Just v' -> normalize v'
    Nothing -> throwError $ "no such symbol: " <> (T.pack . show) sym
normalize exp@STrue = pure exp
normalize (SApp f args) = applyFn f args
normalize exp@(SLambda _ _) = pure exp
normalize (SLet binds bodies) = stackBindsAndRunBody binds bodies
normalize (SDefSym sym sexp) = throwError "can not using define at here"

applyFn ::
  (MonadError T.Text m, HasReader "binds" [(Symbol, SExp)] m) =>
  SExp ->
  [SExp] ->
  m SExp
applyFn f args =
  normalize f >>= \case
    (SLambda requiredArgs body) -> do
      unless (length args == length requiredArgs) $ throwError "incorrect number of arguments"
      let binds = zip requiredArgs args
      stackBindsAndRunBody binds body
    _ -> throwError "not a function"

stackBindsAndRunBody :: (MonadError T.Text m, HasReader "binds" [(Symbol, SExp)] m) => [(Symbol, SExp)] -> NonEmpty SExp -> m SExp
stackBindsAndRunBody binds body = foldr singleLet bodiesRun binds
  where
    singleLet bind bodies' = do
      nb <- mapM normalize bind
      local @"binds" (nb :) bodies'
    bodiesRun = foldr1 (>>) $ normalize <$> body
