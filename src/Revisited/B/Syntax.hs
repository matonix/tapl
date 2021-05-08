{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Revisited.B.Syntax where

import Data.Generics
import Data.Text (Text, pack)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
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
--   > t ::=                     -- term
--   >       (t)
--   >       true
--   >       false
--   >       if t then t else t
--   > v ::=                     -- value
--   >       true
--   >       false

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
    [ parens pTerm,
      pTrue,
      pFalse,
      pIf
    ]

pTrue :: Parser Term
pTrue = TmTrue <$ pKeyword "true"

pFalse :: Parser Term
pFalse = TmFalse <$ pKeyword "false"

pIf :: Parser Term
pIf = TmIf <$ pKeyword "if" <*> pTerm <* pKeyword "then" <*> pTerm <* pKeyword "else" <*> pTerm

-- * Parser Combinators

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

sc :: Parser ()
sc =
  L.space
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

b :: QuasiQuoter
b =
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

-- * Rendering

render :: Term -> TL.Text
render = TLB.toLazyText . renderTerm

renderTerm :: Term -> TLB.Builder
renderTerm TmTrue = TLB.fromText "true"
renderTerm TmFalse = TLB.fromText "false"
renderTerm (TmIf t1 t2 t3) = TLB.fromText "if" <^> t1' <^> TLB.fromText "then" <^> t2' <^> TLB.fromText "else" <^> t3'
  where
    x <^> y = x <> TLB.singleton ' ' <> y
    t1' = renderWithParen t1
    t2' = renderWithParen t2
    t3' = renderWithParen t3

renderWithParen :: Term -> TLB.Builder
renderWithParen t
  | isSingleTerm t = renderTerm t
  | otherwise = TLB.singleton '(' <> renderTerm t <> TLB.singleton ')'

isSingleTerm :: Term -> Bool
isSingleTerm TmTrue = True
isSingleTerm TmFalse = True
isSingleTerm TmIf {} = False
