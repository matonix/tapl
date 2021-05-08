{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Revisited.Untyped.Semantics where

import Control.Exception.Safe
import Data.Text (Text)
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy as TL
import Revisited.Untyped.Syntax

data EvalException = NoRuleApplies deriving (Show)

instance Exception EvalException

-- TODO: 導出木のレンダリング
--       外部からの項の注入（QuasiQuote中の変数）
--       項のパーザの修正（括弧の扱い）
--       doctestでのλのレンダリング（hls-doctestだとPrintできないため不可）

-- | term example

-- >>> c2
-- NTmAbs "s" (NTmAbs "z" (NTmApp (NTmVar "s") (NTmApp (NTmVar "s") (NTmVar "z"))))
-- >>> removeNames [] c2
-- TmAbs "s" (TmAbs "z" (TmApp (TmVar 0 2) (TmApp (TmVar 0 2) (TmVar 1 2))))
-- >>> restoreNames [] $ removeNames [] c2
-- NTmAbs "s" (NTmAbs "z" (NTmApp (NTmVar "s") (NTmApp (NTmVar "s") (NTmVar "z"))))
-- >>> render [] $ restoreNames [] $ removeNames [] c2
-- "\955s. \955z. (s (s z))"
c2 :: NamedTerm
c2 = [uλ|λs. λz. (s (s z))|]


-- >>> removeNames [] sccscc1
-- TmApp (TmAbs "n" (TmAbs "s" (TmAbs "z" (TmApp (TmVar 1 3) (TmApp (TmApp (TmVar 0 3) (TmVar 1 3)) (TmVar 2 3)))))) (TmApp (TmAbs "n" (TmAbs "s" (TmAbs "z" (TmApp (TmVar 1 3) (TmApp (TmApp (TmVar 0 3) (TmVar 1 3)) (TmVar 2 3)))))) (TmAbs "s" (TmAbs "z" (TmApp (TmVar 0 2) (TmVar 1 2)))))
-- >>> result <- eval1 @IO [] $ removeNames [] sccscc1
-- >>> render [] $ restoreNames [] result
-- "(\955n. \955s. \955z. (s ((n s) z)) \955s. \955z. (z ((s z) \955s'. \955z'. (s z))))"
-- >>> result <- eval @IO [] $ removeNames [] sccscc1
-- >>> render [] $ restoreNames [] result
-- "\955s. \955z. (z ((s z) \955s'. \955z'. (z ((s z) \955s''. \955z''. (s z)))))"
sccscc1 :: NamedTerm
sccscc1 = [uλ|(λn. λs. λz. (s ((n s) z)) (λn. λs. λz. (s ((n s) z)) λs. λz. (s z)))|]

-- * shift and substitution

-- |
-- \[
--   \uparrow^d(t)
-- \]
termShift :: Int -> Term -> Term
termShift d t = tmmap
  (\c x n -> TmVar (x + (if x >= c then d else 0)) (n + d)) 0 t

-- |
-- \[
--   [j \mapsto s]t
-- \]
termSubst :: Int -> Term -> Term -> Term
termSubst j s t = tmmap
  (\c x n -> if x == j + c then termShift c s else TmVar x n) 0 t

-- | E-AppAbs:
termSubstTop :: Term -> Term -> Term
termSubstTop s t =
  termShift (-1) (termSubst 0 (termShift 1 s) t)

-- ** helper functions

tmmap :: (Int -> Int -> Int -> Term) -> Int -> Term -> Term
tmmap onvar = walk where
  walk c (TmVar x n) = onvar c x n
  walk c (TmAbs x t1) = TmAbs x (walk (c + 1) t1)
  walk c (TmApp t1 t2) = TmApp (walk c t1) (walk c t2)

-- * evaluation

-- | 1-step evaluation
eval1 :: MonadThrow m => Context -> Term -> m Term
-- E-AppAbs
eval1 ctx (TmApp (TmAbs _x t12) v2) | isval ctx v2 =
  return $ termSubstTop v2 t12
-- E-App2
eval1 ctx (TmApp v1 t2) | isval ctx v1 = do
  t2' <- eval1 ctx t2
  return $ TmApp v1 t2'
-- E-App1
eval1 ctx (TmApp t1 t2) = do
  t1' <- eval1 ctx t1
  return $ TmApp t1' t2
eval1 _ _ =
  throw NoRuleApplies

-- | multi-step evaluation
eval :: MonadCatch m => Context -> Term -> m Term
eval ctx t = do
  e <- try $ eval1 ctx t
  case e of
    Left NoRuleApplies -> return t
    Right t' -> eval ctx t'

-- ** helper functions
isval :: Context -> Term -> Bool
isval _ TmAbs{} = True
isval _ _ = False
