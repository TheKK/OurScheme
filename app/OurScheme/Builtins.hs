{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module OurScheme.Builtins
  ( stdBuiltins,
  )
where

import Control.Monad.Except
import Data.Foldable
import qualified Data.Text as T
import OurScheme.AST

stdBuiltins :: [(Symbol, SExp)]
stdBuiltins =
  fmap SBuiltin
    <$> [ (Symbol "add", builtinAdd),
          (Symbol "minus", builtinMinus),
          (Symbol "lt", builtinLessThan)
        ]

builtinAdd :: BuiltinsImpl
builtinAdd = BuiltinsImpl $ \args' -> do
  args <- mapM getInt args'
  pure $ SLit $ LitInt $ sum args

builtinMinus :: BuiltinsImpl
builtinMinus = BuiltinsImpl $ \args' -> do
  args <- mapM getInt args'
  if null args
    then pure $ SLit $ LitInt 0
    else do
      let n : ns = args
      pure $ SLit $ LitInt $ foldl' (-) n ns

builtinLessThan :: BuiltinsImpl
builtinLessThan = BuiltinsImpl $ \args' -> do
  when (length args' /= 2) $ Left "wrong number of arguments"
  args <- mapM getInt args'
  let (a : b : _) = args
  pure $ if a < b then STrue else SNil

getInt :: SExp -> Either T.Text Int
getInt (SLit (LitInt i)) = pure i
getInt _ = throwTypeError "int"

throwTypeError :: MonadError T.Text m => T.Text -> m a
throwTypeError = throwError . ("type error: " <>)
{-# INLINE throwTypeError #-}
