module OurScheme.AST
  ( SExp (..),
    Literal (..),
    Symbol (..),
    BuiltinsImpl (..),
  )
where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as T

data SExp
  = SLit Literal
  | SSym Symbol
  | STrue
  | SNil
  | SApp SExp [SExp]
  | SLambda [Symbol] (NonEmpty SExp)
  | SBuiltin BuiltinsImpl
  | SLet [(Symbol, SExp)] (NonEmpty SExp)
  | SIf SExp SExp SExp
  | SDefSym Symbol SExp
  deriving (Show)

data Literal = LitInt Int | LitText T.Text
  deriving (Eq)

instance Show Literal where
  show (LitInt l) = show l
  show (LitText l) = T.unpack l

newtype Symbol = Symbol T.Text
  deriving (Show, Eq)

newtype BuiltinsImpl = BuiltinsImpl ([SExp] -> Either T.Text SExp)

instance Show BuiltinsImpl where
  show _ = "builtin"
