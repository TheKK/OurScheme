{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
module OurScheme.Eval () where

import GHC.Generics
import Control.Monad.Identity
import Control.Monad.Reader (runReader, ReaderT (..))
import Control.Monad.Except
import Data.List.NonEmpty
import qualified Data.Text as T
import Capability.Reader
import Capability.Source

import OurScheme.AST

data Env = Env
 { envBinds :: [(Symbol, SExp)]
 }
 deriving (Generic)

blankEnv :: Env
blankEnv = Env []

newtype EnvT m a = EnvT (ReaderT Env (ExceptT T.Text m) a)
  deriving (Functor, Applicative, Monad)
  deriving (MonadError T.Text)
  deriving (HasReader "binds" [(Symbol, SExp)], HasSource "binds" [(Symbol, SExp)])
    via (Rename "envBinds" (Field "envBinds" () (MonadReader (ReaderT Env (ExceptT T.Text m)))))

runEnv ::  EnvT Identity a -> Either T.Text a
runEnv (EnvT m) = runExcept $ runReaderT m blankEnv

data Result = RInt Int | RText T.Text

instance Show Result where
  show (RInt l) = show l
  show (RText l) = show l

eval :: SExp -> Either T.Text Result
eval = runEnv . eval'

eval' :: (MonadError T.Text m, HasReader "binds" [(Symbol, SExp)] m) => SExp -> m Result
eval' s@(SLit _) = pure $ sexpToResult s
eval' (SSym sym)= do
    v <- reader @"binds" $ lookup sym
    case v of
      Just v' -> pure $ sexpToResult v'
      Nothing -> throwError $ "no such symbol: " <> (T.pack . show) sym
eval' (SLet binds bodies) = local @"binds" (binds <>) $ foldr1 (>>) $ fmap eval' $ bodies

sexpToResult :: SExp -> Result
sexpToResult (SLit (LitInt i))= RInt i
sexpToResult (SLit (LitText t))= RText t

go = putStrLn $ T.unpack $ either ("Error: " <>) (T.pack . show) $ eval $
  SLet [(Symbol "x", (SLit $ LitText "88"))] $
    (SLit $ LitInt 10) :|
      [ (SLit $ LitInt 11)
      , (SSym $ Symbol "gg")
      , (SSym $ Symbol "x")
      ]
