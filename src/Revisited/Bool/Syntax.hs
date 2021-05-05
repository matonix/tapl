{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Revisited.Bool.Syntax where

import Data.Generics
import Data.Text (Text, pack)
import Data.Void (Void)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L 

-- QQ 参考：
--   https://ruicc.hatenablog.jp/entry/20111015/1318630433
--   https://wiki.haskell.org/Quasiquotation
-- Megaparsec:
--   https://haskell.e-bigmoon.com/posts/2019/07-14-megaparsec-tutorial.html

-- | Untyped Bool Syntax:
--    >t ::=                     -- term
--    >      true
--    >      false
--    >      if t then t else t
--    >      (t)
--    >v ::=                     -- value
--    >      true
--    >      false

-- * Parsing

type Parser = Parsec Void Text

data Term
  = TmTrue
  | TmFalse
  | TmIf Term Term Term
  deriving (Show, Eq, Typeable, Data)

pTerm :: Parser Term
pTerm =
  choice
    [ pTrue,
      pFalse,
      pIf,
      parens pTerm
    ]

pTrue :: Parser Term
pTrue = TmTrue <$ pKeyword "true"

pFalse :: Parser Term
pFalse = TmFalse <$ pKeyword "false"

pIf :: Parser Term
pIf = TmIf <$ pKeyword "if" <*> pTerm <* pKeyword "then" <*> pTerm <* pKeyword "else" <*> pTerm

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "--")
  (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

pKeyword :: Text -> Parser Text
pKeyword keyword = lexeme (string keyword <* notFollowedBy alphaNumChar)

-- * Quasiquoting

bool :: QuasiQuoter
bool =
  QuasiQuoter
    { quoteExp = parseExp,
      quotePat = undefined,
      quoteType = undefined,
      quoteDec = undefined
    }

parseExp :: String -> Q Exp
parseExp str = do
  expr <- case runParser pTerm "" (pack str) of
    Left err -> fail $ errorBundlePretty err
    Right e -> return e
  dataToExpQ (const Nothing) expr
