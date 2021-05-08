{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Revisited.NB.Syntax where

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

-- | Untyped Number Syntax with Bool:
--   > t  ::=                     -- term
--   >        (t)
--   >        true
--   >        false
--   >        if t then t else t
--   >        0
--   >        succ t
--   >        pred t
--   >        iszero t
--   > v  ::=                     -- value
--   >        true
--   >        false
--   >        nv
--   > nv ::=                     -- numeric value
--   >        0
--   >        succ nv

-- * Parsing

type Parser = Parsec Void Text

data Term
  = TmTrue
  | TmFalse
  | TmIf Term Term Term
  | TmZero
  | TmSucc Term
  | TmPred Term
  | TmIszero Term
  deriving (Show, Eq, Typeable, Data)

pTerm :: Parser Term
pTerm =
  choice
    [ parens pTerm,
      pTrue,
      pFalse,
      pIf,
      pZero,
      pSucc,
      pPred,
      pIszero
    ]

pTrue :: Parser Term
pTrue = TmTrue <$ pKeyword "true"

pFalse :: Parser Term
pFalse = TmFalse <$ pKeyword "false"

pIf :: Parser Term
pIf = TmIf <$ pKeyword "if" <*> pTerm <* pKeyword "then" <*> pTerm <* pKeyword "else" <*> pTerm

pZero :: Parser Term
pZero = TmZero <$ lexeme (L.decimal :: Parser Integer)

pSucc :: Parser Term
pSucc = TmSucc <$ pKeyword "succ" <*> pTerm

pPred :: Parser Term
pPred = TmPred <$ pKeyword "pred" <*> pTerm

pIszero :: Parser Term
pIszero = TmIszero <$ pKeyword "iszero" <*> pTerm

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

nb :: QuasiQuoter
nb =
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
renderTerm (TmIf t1 t2 t3) =
  TLB.fromText "if"
    <^> renderWithParen t1
    <^> TLB.fromText "then"
    <^> renderWithParen t2
    <^> TLB.fromText "else"
    <^> renderWithParen t3
renderTerm TmZero = TLB.fromText "0"
renderTerm (TmSucc t1) =
  TLB.fromText "succ"
    <^> renderWithParen t1
renderTerm (TmPred t1) =
  TLB.fromText "pred"
    <^> renderWithParen t1
renderTerm (TmIszero t1) =
  TLB.fromText "iszero"
    <^> renderWithParen t1

renderWithParen :: Term -> TLB.Builder
renderWithParen t
  | isSingleTerm t = renderTerm t
  | otherwise = TLB.singleton '(' <> renderTerm t <> TLB.singleton ')'

isSingleTerm :: Term -> Bool
isSingleTerm TmTrue = True
isSingleTerm TmFalse = True
isSingleTerm TmZero = True
isSingleTerm _ = False

(<^>) :: TLB.Builder -> TLB.Builder -> TLB.Builder
x <^> y = x <> TLB.singleton ' ' <> y