{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module OurScheme.Eval
  ( topLevelNormalize,
  )
where

import Capability.Reader
import Capability.State
import Control.Monad.Except
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Text as T
import OurScheme.AST

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
normalize exp@SNil = pure exp
normalize (SApp f args) = applyFn f args
normalize exp@(SLambda _ _) = pure exp
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
