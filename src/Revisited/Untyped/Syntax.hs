{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Revisited.Untyped.Syntax where

import Data.Generics
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import Data.Void (Void)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- | Untyped Lambda:
-- パーサを簡単にするためAppにて括弧を義務化
--   > t  ::=                     -- term
--   >        x
--   >        λx.t
--   >        (t t)
--   > v  ::=                     -- value
--   >        λx.t

-- * Parsing

type Parser = Parsec Void Text

-- | Named representation
data NamedTerm
  = NTmVar Text
  | NTmAbs Text NamedTerm
  | NTmApp NamedTerm NamedTerm
  deriving (Show, Eq, Typeable, Data)

-- | Unnamed representation
data Term
  = TmVar Int Int -- deBruijnIndex lengthOfCurrentContext
  | TmAbs Text Term -- hintNameOfBindingVariable term
  | TmApp Term Term -- term1 term2
  deriving (Show, Eq, Typeable, Data)

type Context = [(Text, Binding)]

data Binding = NameBind
  deriving (Eq, Show)

pTerm :: Parser NamedTerm
pTerm =
  choice
    [ parens pApp,
      pAbs,
      pVar
    ]

pVar :: Parser NamedTerm
pVar = NTmVar <$> pIdentifier <?> "var"

pAbs :: Parser NamedTerm
pAbs = NTmAbs <$ symbol "λ" <*> pIdentifier <* symbol "." <*> pTerm <?> "abs"

pApp :: Parser NamedTerm
pApp = NTmApp <$> pTerm <*> pTerm <?> "app"

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
pKeyword keyword = lexeme (string keyword <* notFollowedBy alphaNumChar <?> T.unpack ("keyword:" <> keyword))

pIdentifier :: Parser Text
pIdentifier = lexeme (T.cons <$> latin1Char <*> (T.pack <$> many alphaNumChar) <?> "identifier")

-- * Transforming Named/Unnamed Terms

removeNames :: Context -> NamedTerm -> Term
removeNames ctx (NTmVar x) = TmVar (last $ findIndices ((== x) . fst) ctx) (length ctx)
removeNames ctx (NTmAbs x t1) = TmAbs x $ removeNames (ctx ++ [(x, NameBind)]) t1
removeNames ctx (NTmApp t1 t2) = TmApp (removeNames ctx t1) (removeNames ctx t2)

restoreNames :: Context -> Term -> NamedTerm
restoreNames ctx (TmVar x _) = NTmVar (fst $ ctx !! x)
restoreNames ctx (TmAbs x t1) = NTmAbs x' $ restoreNames (ctx ++ [(x', NameBind)]) t1
  where
    x' =
      if elem x $ map fst ctx
        then let (_, x'') = pickfreshname ctx x in x''
        else x
restoreNames ctx (TmApp t1 t2) = NTmApp (restoreNames ctx t1) (restoreNames ctx t2)

-- prop> \t -> removeNames [] (restoreNames [] t) == t
-- No instance for (Arbitrary Term)
--   arising from a use of ‘propEvaluation’

-- * Quasiquoting

uλ :: QuasiQuoter
uλ =
  QuasiQuoter
    { quoteExp = parseExp,
      quotePat = undefined,
      quoteType = undefined,
      quoteDec = undefined
    }

-- Text を ExpQ に変換する際に問題が発生するので以下方法で対処
-- https://stackoverflow.com/questions/38143464/cant-find-inerface-file-declaration-for-variable
liftText :: Text -> Q Exp
liftText txt = AppE (VarE 'T.pack) <$> lift (T.unpack txt)

parseExp :: String -> Q Exp
parseExp str = do
  expr <- case runParser pTerm "" (T.pack str) of
    Left err -> fail $ errorBundlePretty err
    Right e -> return e
  dataToExpQ (fmap liftText . cast) expr

-- * Rendering

render :: Context -> NamedTerm -> TL.Text
render ctx = TLB.toLazyText . renderTerm ctx

renderTerm :: Context -> NamedTerm -> TLB.Builder
renderTerm _ctx (NTmVar x) = TLB.fromText x
renderTerm ctx (NTmAbs x t1) =
  let (ctx', x') = pickfreshname ctx x
   in TLB.singleton 'λ' <> TLB.fromText x' <> TLB.singleton '.' <^> renderTerm ctx' t1
renderTerm ctx (NTmApp t1 t2) =
  TLB.singleton '(' <> renderTerm ctx t1 <^> renderTerm ctx t2 <> TLB.singleton ')'

(<^>) :: TLB.Builder -> TLB.Builder -> TLB.Builder
x <^> y = x <> TLB.singleton ' ' <> y

-- ** helper functions

ctxlength :: Context -> Int
ctxlength = subtract 1 . length

index2name :: Context -> Int -> TLB.Builder
index2name ctx ix = TLB.fromText $ fst (ctx !! ix)

pickfreshname :: Context -> Text -> (Context, Text)
pickfreshname ctx x =
  if x `elem` map fst ctx
    then pickfreshname ctx (x <> "'")
    else ((x, NameBind) : ctx, x)
