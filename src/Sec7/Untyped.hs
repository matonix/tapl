module Sec7.Untyped where

import Data.Semigroup ((<>))
import Control.Exception

data NoRuleApplies = NoRuleApplies deriving (Show)

instance Exception NoRuleApplies

data Term
  = TmVar Info Int Int -- info deBruijnIndex lengthOfCurrentContext
  | TmAbs Info String Term -- info hintNameOfBindingVariable term
  | TmApp Info Term Term -- indo term1 term2

data Info = Info

type Context = [(Printable, Binding)]

data Binding = NameBind

type PP = Either String Printable

type Printable = String

-- * put

-- | put String notation
printtm :: Context -> Term -> PP
printtm ctx (TmVar fi x n) = if ctxlength ctx == n
  then Right (index2name fi ctx x)
  else Left "[bad index]"
printtm ctx (TmAbs _fi x t1) = let (ctx', x') = pickfreshname ctx x in
  Right ("(λ " ++ x' ++ ". ") <> printtm ctx' t1 <> Right ")"
printtm ctx (TmApp _fi t1 t2) =
  Right "(" <> printtm ctx t1 <> printtm ctx t2 <> Right ")"

-- ** helper functions

ctxlength :: Context -> Int
ctxlength = subtract 1 . length

index2name :: Info -> Context -> Int -> Printable
index2name _fi ctx ix = fst (ctx !! ix)

pickfreshname :: Context -> Printable -> (Context, Printable)
pickfreshname ctx x = if x `elem` map fst ctx
  then pickfreshname ctx (x ++ "'")
  else ((x, NameBind) : ctx, x)

-- * shift and substitution

-- |
-- \[
--   \uparrow^d(t)
-- \]
termShift :: Int -> Term -> Term
termShift d t = tmmap
  (\fi c x n -> TmVar fi (x + (if x >= c then d else 0)) (n + d)) 0 t

-- |
-- \[
--   [j \mapsto s]t
-- \]
termSubst :: Int -> Term -> Term -> Term
termSubst j s t = tmmap
  (\fi c x n -> if x == j + c then termShift c s else TmVar fi x n) 0 t

-- | E-AppAbs:
termSubstTop :: Term -> Term -> Term
termSubstTop s t =
  termShift (-1) (termSubst 0 (termShift 1 s) t)

-- ** helper functions

tmmap :: (Info -> Int -> Int -> Int -> Term) -> Int -> Term -> Term
tmmap onvar = walk where
  walk c (TmVar fi x n) = onvar fi c x n
  walk c (TmAbs fi x t1) = TmAbs fi x (walk (c + 1) t1)
  walk c (TmApp fi t1 t2) = TmApp fi (walk c t1) (walk c t2)

-- * evaluation

-- | 1-step evaluation
eval1 :: Context -> Term -> Term
-- E-AppAbs
eval1 ctx (TmApp _fi (TmAbs _ _x t12) v2) | isval ctx v2 =
  termSubstTop v2 t12
-- E-App2
eval1 ctx (TmApp fi v1 t2) | isval ctx v1 =
  let t2' = eval1 ctx t2 in
  TmApp fi v1 t2'
-- E-App1
eval1 ctx (TmApp fi t1 t2) =
  let t1' = eval1 ctx t1 in
  TmApp fi t1' t2
eval1 _ctx _ =
  throw NoRuleApplies

-- | multi-step evaluation
eval :: Context -> Term -> IO Term
eval ctx t = do
  t' <- try (evaluate (eval1 ctx t))
  case t' of
    Left NoRuleApplies -> return t
    Right t'' -> eval ctx t''

-- ** helper functions
isval :: Context -> Term -> Bool
isval _ TmAbs{} = True
isval _ _ = False
