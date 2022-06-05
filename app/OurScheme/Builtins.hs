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
    <$> [ (Symbol "+", builtinAdd),
          (Symbol "-", builtinMinus),
          (Symbol "<", builtinBinaryCondition (<)),
          (Symbol "<=", builtinBinaryCondition (<=)),
          (Symbol ">", builtinBinaryCondition (>)),
          (Symbol ">=", builtinBinaryCondition (>=)),
          (Symbol "=", builtinBinaryCondition (==))
        ]

builtinAdd :: BuiltinsImpl
builtinAdd = BuiltinsImpl $ \args' -> do
  args <- mapM getInt args'
  pure $ SLit $ LitInt $ sum args

builtinMinus :: BuiltinsImpl
builtinMinus = BuiltinsImpl $ \args' -> do
  args <- mapM getInt args'
  pure $
    SLit $
      LitInt $ case args of
        [] -> 0
        [n] -> - n
        n : ns -> foldl' (-) n ns

builtinBinaryCondition :: (Int -> Int -> Bool) -> BuiltinsImpl
builtinBinaryCondition f = builtinBinaryArithmetic $ \a b ->
  if f a b then STrue else SNil

builtinBinaryArithmetic :: (Int -> Int -> SExp) -> BuiltinsImpl
builtinBinaryArithmetic f = BuiltinsImpl $ \args' -> do
  when (length args' /= 2) $ Left "wrong number of arguments"
  args <- mapM getInt args'
  let (a : b : _) = args
  pure $ f a b

getInt :: SExp -> Either T.Text Int
getInt (SLit (LitInt i)) = pure i
getInt _ = throwTypeError "int"

throwTypeError :: MonadError T.Text m => T.Text -> m a
throwTypeError = throwError . ("type error: " <>)
{-# INLINE throwTypeError #-}
