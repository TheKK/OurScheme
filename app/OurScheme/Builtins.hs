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
    <$> [ (Symbol "+", builtinFoldInts sum),
          (Symbol "*", builtinFoldInts product),
          (Symbol "-", builtinMinus),
          (Symbol "/", builtinDivision),
          (Symbol "<", builtinBinaryCondition (<)),
          (Symbol "<=", builtinBinaryCondition (<=)),
          (Symbol ">", builtinBinaryCondition (>)),
          (Symbol ">=", builtinBinaryCondition (>=)),
          (Symbol "=", builtinBinaryCondition (==)),
          (Symbol "and", builtinAnd),
          (Symbol "or", builtinOr)
        ]

builtinMinus :: BuiltinsImpl
builtinMinus = BuiltinsImpl $ \args' -> do
  args <- mapM getInt args'
  pure $
    SLit $
      LitInt $ case args of
        [] -> 0
        [n] -> -n
        n : ns -> foldl' (-) n ns

builtinDivision :: BuiltinsImpl
builtinDivision = BuiltinsImpl $ \args' -> do
  args <- mapM (fmap fromIntegral . getInt) args'
  ret <- case args of
    [] -> Left "wrong number of arguments"
    [n] -> pure $ 1 / n
    n : ns -> pure $ foldl' (/) n ns
  if isInfinite ret
    then Left "arithmetic error, divide by zero"
    else pure $ SLit $ LitInt $ floor ret

builtinFoldInts :: ([Int] -> Int) -> BuiltinsImpl
builtinFoldInts f = BuiltinsImpl $ \args' -> do
  args <- mapM getInt args'
  pure $ SLit $ LitInt $ f args

builtinAnd :: BuiltinsImpl
builtinAnd = BuiltinsImpl $ pure . foldl' sExpAnd STrue

builtinOr :: BuiltinsImpl
builtinOr = BuiltinsImpl $ pure . foldl' sExpOr SNil

builtinBinaryCondition :: (Int -> Int -> Bool) -> BuiltinsImpl
builtinBinaryCondition f = builtinBinaryArithmetic $ \a b ->
  if f a b then STrue else SNil

builtinBinaryArithmetic :: (Int -> Int -> SExp) -> BuiltinsImpl
builtinBinaryArithmetic f = BuiltinsImpl $ \args' -> do
  when (length args' /= 2) $ Left "wrong number of arguments"
  args <- mapM getInt args'
  let (a : b : _) = args
  pure $ f a b

sExpAnd :: SExp -> SExp -> SExp
sExpAnd l r = if isNil l then SNil else r

sExpOr :: SExp -> SExp -> SExp
sExpOr l r = if not $ isNil l then l else r

getInt :: SExp -> Either T.Text Int
getInt (SLit (LitInt i)) = pure i
getInt _ = throwTypeError "int"

isNil :: SExp -> Bool
isNil SNil = True
isNil _ = False

throwTypeError :: MonadError T.Text m => T.Text -> m a
throwTypeError = throwError . ("type error: " <>)
{-# INLINE throwTypeError #-}
