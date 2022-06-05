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
      pTrue,
      pNil,
      SSym <$> pSymbol,
      try pLet,
      try pIf,
      try pLambda,
      try pDefSym,
      try pApp
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

pLambda :: Parser SExp
pLambda = lexeme $ parens (pKeyword "lambda" >> SLambda <$> parens (many pSymbol) <*> NE.some pSExp <?> "LAMBDA")

pApp :: Parser SExp
pApp = lexeme $ parens (SApp <$> pSExp <*> many pSExp <?> "FUNC_APP")

pLet :: Parser SExp
pLet = lexeme $ parens ((pKeyword "let" >> SLet <$> parens pBinds <*> (pBody <?> "BODY")) <?> "LET")
  where
    pBinds = many (parens ((,) <$> lexeme pSymbol <*> pSExp) <?> "(SYMBOL SEXP)")
    pBody = NE.some pSExp

pIf :: Parser SExp
pIf = lexeme $ parens ((pKeyword "if" >> SIf <$> pSExp <*> pSExp <*> pSExp) <?> "IF")

pDefSym :: Parser SExp
pDefSym = lexeme $ parens (pKeyword "define" >> SDefSym <$> pSymbol <*> pSExp <?> "DEFINE")

pTrue :: Parser SExp
pTrue = lexeme $ STrue <$ choice [pKeyword "t", pKeyword "#t"]

pNil :: Parser SExp
pNil = lexeme $ SNil <$ choice [pKeyword "nil", pKeyword "#f", pKeyword "()"]

pSymbol :: Parser Symbol
pSymbol = lexeme $ Symbol . T.pack <$> (some C.alphaNumChar <* notFollowedBy C.alphaNumChar <?> "SYMBOL")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

alphaChar :: Parser Char
alphaChar = satisfy isAlpha <?> "alpha character"

space :: Parser ()
space = L.space C.space1 (L.skipLineComment "--") mzero

pKeyword :: T.Text -> Parser T.Text
pKeyword p = lexeme (C.string p <* notFollowedBy C.alphaNumChar)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space
{-# INLINE lexeme #-}

symbol :: T.Text -> Parser T.Text
symbol = L.symbol space
{-# INLINE symbol #-}
