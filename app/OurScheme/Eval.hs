{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module OurScheme.Eval
  ( topLevelNormalize,
    Binds (..),
    Builtins (..),
  )
where

import Capability.Reader
import Capability.State
import Control.Applicative
import Control.Monad.Except
import Data.Function
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Text as T
import OurScheme.AST

data Binds

data Builtins

type instance TypeOf Type Binds = [(Symbol, SExp)]

type instance TypeOf Type Builtins = [(Symbol, SExp)]

topLevelNormalize :: (MonadError T.Text m, HasState' Binds m, HasReader' Binds m, HasReader' Builtins m) => SExp -> m SExp
topLevelNormalize exp@(SDefSym sym sexp) = normalize sexp >>= \sexp' -> modify' @Binds ((sym, sexp') :) >> pure sexp'
topLevelNormalize other = normalize other

normalize :: (MonadError T.Text m, HasReader' Binds m, HasReader' Builtins m) => SExp -> m SExp
normalize exp@(SLit _) = pure exp
normalize (SSym sym) = do
  builtin <- reader @Builtins $ lookup sym
  bind <- reader @Binds $ lookup sym
  case builtin <|> bind of
    Just v' -> normalize v'
    Nothing -> throwError $ "no such symbol: " <> (T.pack . show) sym
normalize exp@STrue = pure exp
normalize exp@SNil = pure exp
normalize (SApp f args) = applyFn f args
normalize exp@(SLambda _ _) = pure exp
normalize exp@(SBuiltin _) = pure exp
normalize (SLet binds bodies) = stackBindsAndRunBody binds bodies
normalize (SIf cond' t' nil') = do
  cond <- normalize cond'
  normalize $ if isNil cond then nil' else t'
normalize (SDefSym sym sexp) = throwError "can not using define at here"

-- TODO Now we have some potential issue: we watn input SExp be normalized
-- but we can't distinguish them now. Maybe wrapping them in newtype would work.
isNil :: SExp -> Bool
isNil SNil = True
isNil _ = False

applyFn ::
  (MonadError T.Text m, HasReader' Binds m, HasReader' Builtins m) =>
  SExp ->
  [SExp] ->
  m SExp
applyFn f args =
  normalize f >>= \case
    (SLambda requiredArgs body) -> do
      unless (length args == length requiredArgs) $ throwError "incorrect number of arguments"
      let binds = zip requiredArgs args
      stackBindsAndRunBody binds body
    (SBuiltin (BuiltinsImpl impl)) ->
      mapM normalize args
        >>= ( \normArgs ->
                impl normArgs & \case
                  Left e -> throwError e
                  Right v -> pure v
            )
        >>= normalize
    _ -> throwError "not a function"

stackBindsAndRunBody :: (MonadError T.Text m, HasReader' Binds m, HasReader' Builtins m) => [(Symbol, SExp)] -> NonEmpty SExp -> m SExp
stackBindsAndRunBody binds body = foldr singleLet bodiesRun binds
  where
    singleLet bind bodies' = do
      nb <- mapM normalize bind
      local @Binds (nb :) bodies'
    bodiesRun = foldr1 (>>) $ normalize <$> body
