module OurScheme.AST
  ( SExp(..)
  , Literal(..)
  , Symbol(..)
  ) where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty

import qualified Data.Text as T

data SExp
  = SLit Literal
  | SSym Symbol
  | SLet [(Symbol, SExp)] (NonEmpty SExp)
  | SDefSym Symbol SExp
  deriving (Show)

data Literal = LitInt Int | LitText T.Text
  deriving (Eq)

instance Show Literal where
  show (LitInt l) = show l
  show (LitText l) = T.unpack l

newtype Symbol = Symbol T.Text
  deriving (Show, Eq)
