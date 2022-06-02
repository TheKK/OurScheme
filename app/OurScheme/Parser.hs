{-# LANGUAGE OverloadedStrings #-}

module OurScheme.Parser () where

import Control.Monad
import qualified Control.Monad.Combinators.NonEmpty as NE
import Data.Char (isAlpha)
import qualified Data.Text as T
import Data.Void
import OurScheme.AST
import Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import Text.ParserCombinators.ReadP (many1)

type Parser a = Parsec Void T.Text a

pSExp :: Parser SExp
pSExp =
  choice
    [ pLiteral,
      SSym <$> pSymbol,
      pLet
    ]

pLiteral :: Parser SExp
pLiteral =
  lexeme $
    SLit
      <$> choice
        [ pLitInt <?> "INT-LIT",
          pLitText <?> "STRING-LIT"
        ]
  where
    pLitInt = LitInt <$> L.decimal
    pLitText = LitText . T.pack <$> (symbol "\"" >> manyTill C.asciiChar (symbol "\""))

pLet :: Parser SExp
pLet = parens $ lexeme "let" >> SLet <$> parens pBinds <*> (pBody <?> "BODY")
  where
    pBinds = many (parens ((,) <$> pSymbol <*> pSExp) <?> "(SYMBOL SEXP)")
    pBody = NE.some pSExp

pSymbol :: Parser Symbol
pSymbol = lexeme (Symbol . T.pack <$> some (alphaChar <|> C.char '?')) <?> "SYMBOL"

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

alphaChar :: Parser Char
alphaChar = satisfy isAlpha <?> "alpha character"

space :: Parser ()
space = L.space C.space1 (L.skipLineComment "--") mzero

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space
{-# INLINE lexeme #-}

symbol :: T.Text -> Parser T.Text
symbol = L.symbol space
{-# INLINE symbol #-}
