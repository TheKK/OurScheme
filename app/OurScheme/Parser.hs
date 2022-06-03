{-# LANGUAGE OverloadedStrings #-}

module OurScheme.Parser where

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
      pLet,
      pDefSym
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
pLet = parens (lexeme1 "let" >> SLet <$> parens pBinds <*> (pBody <?> "BODY")) <?> "LET"
  where
    pBinds = many (parens ((,) <$> lexeme pSymbol <*> pSExp) <?> "(SYMBOL SEXP)")
    pBody = NE.some pSExp

pDefSym :: Parser SExp
pDefSym = parens (lexeme1 "define" >> SDefSym <$> lexeme1 pSymbol <*> pSExp <?> "DEFINE")

pSymbol :: Parser Symbol
pSymbol = Symbol . T.pack <$> some C.alphaNumChar <?> "SYMBOL"

parens :: Parser a -> Parser a
parens = try . between (symbol "(") (symbol ")")

alphaChar :: Parser Char
alphaChar = satisfy isAlpha <?> "alpha character"

space :: Parser ()
space = L.space C.space1 (L.skipLineComment "--") mzero

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space
{-# INLINE lexeme #-}

lexeme1 :: Parser a -> Parser a
lexeme1 p = L.lexeme space $ p <* " "
{-# INLINE lexeme1 #-}

symbol :: T.Text -> Parser T.Text
symbol = L.symbol space
{-# INLINE symbol #-}
