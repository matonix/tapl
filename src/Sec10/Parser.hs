module Sec10.Parser where

import Data.List
import Data.Maybe
import Sec10.SimplyTyped
import Data.Void
import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- | ref: https://markkarpov.com/megaparsec/parsing-simple-imperative-language.html

type Parser = Parsec Void String

{- |
  The grammar
  t ::= x | λx:T.t | t t | true | false | if t then t else t | (t)
  T ::= T→T | Bool
  Γ ::= ∅ | Γ,x:T
  J ::= Γ⊢t:T
-}

type Judgement = (Context, NamedTerm, Ty)

data NamedTerm
  = NTmVar Info Printable
  | NTmAbs Info Printable Ty NamedTerm
  | NTmApp Info NamedTerm NamedTerm
  | NTmTrue Info
  | NTmFalse Info
  | NTmIf Info NamedTerm NamedTerm NamedTerm
  deriving (Eq, Show)

-- * transform

removenames :: Judgement -> Term
removenames (ctx, NTmVar fi name, _typ) =
  TmVar fi (fromJust (findIndex ((==name) . fst) ctx)) (length ctx)
removenames (ctx, NTmAbs fi name typ1 t1, typ2) =
  TmAbs fi name typ1 (removenames ((name, VarBind typ1):ctx, t1, typ2))
removenames (ctx, NTmApp fi t1 t2, typ) =
  TmApp fi (removenames (ctx, t1, typ)) (removenames (ctx, t2, typ))
removenames (_ctx, NTmTrue fi, _typ) =
  TmTrue fi
removenames (_ctx, NTmFalse fi, _typ) =
  TmFalse fi
removenames (ctx ,NTmIf fi t1 t2 t3, typ) = 
  TmIf fi (removenames (ctx, t1, typ)) 
    (removenames (ctx, t2, typ)) (removenames (ctx, t3, typ))

fromString :: String -> Maybe Judgement
fromString = parseMaybe judgement

-- * Lexer

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

lambda :: Parser String
lambda = symbol "\\"

vdash :: Parser String
vdash = symbol "⊢"

colon :: Parser String
colon = symbol ":"

dot :: Parser String
dot = symbol "."

comma :: Parser String
comma = symbol ","

arrow :: Parser String
arrow = symbol "→"

rword :: String -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

rws :: [String] -- list of reserved words
rws = ["if","then","else","true","false","Bool"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rws
      then fail $ "keyword " ++ show x ++ " cannot be an identifier"
      else return x

-- * Parser

whileParser :: Parser NamedTerm
whileParser = between sc eof appTerm

-- | J ::= Γ⊢t:T

judgement :: Parser Judgement
judgement = (,,)
  <$> context
  <* vdash <*> appTerm
  <* colon <*> arrowTy

-- | t ::= x | λx:T.t | true | false | if t then t else t | (t) | t t

-- | call this
appTerm :: Parser NamedTerm
appTerm = makeExprParser term [[InfixR (NTmApp Info <$ space)]]

term :: Parser NamedTerm
term = parens appTerm
  <|> varTerm
  <|> absTerm
  <|> trueTerm
  <|> falseTerm
  <|> ifTerm

varTerm :: Parser NamedTerm
varTerm = NTmVar Info
  <$> identifier

absTerm :: Parser NamedTerm
absTerm = NTmAbs Info
  <$ lambda <*> identifier
  <* colon <*> arrowTy
  <* dot <*> appTerm

trueTerm :: Parser NamedTerm
trueTerm = NTmTrue Info
  <$ rword "true"

falseTerm :: Parser NamedTerm
falseTerm = NTmFalse Info
  <$ rword "false"

ifTerm :: Parser NamedTerm
ifTerm = NTmIf Info
  <$ rword "if" <*> appTerm
  <* rword "then" <*> appTerm
  <* rword "else" <*> appTerm

-- | T ::= T→T | Bool | (T)

-- | call this
arrowTy :: Parser Ty
arrowTy = makeExprParser ty [[InfixR (TyArr <$ arrow)]]

ty :: Parser Ty
ty = parens arrowTy
  <|> boolTy

boolTy :: Parser Ty
boolTy = TyBool <$ rword "Bool"

-- | Γ ::= ∅ | Γ,x:T

context :: Parser Context
context = relation `sepBy` comma

relation :: Parser (Printable, Binding)
relation = (,)
  <$> identifier
  <* colon <*> binding

binding :: Parser Binding
binding = VarBind <$> arrowTy