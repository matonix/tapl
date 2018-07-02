module Sec7.Ex7_3_1 where

import Sec7.Untyped
import Control.Exception

-- | reference: p.54 & Ex5.3.8 (p.398)

-- * evaluation

-- | big-step evaluation (naive)
evalBig :: Context -> Term -> Term
evalBig ctx (TmApp _fi t1 t2) = let
  t1' = evalBig ctx t1
  t2' = evalBig ctx t2
  v' = termSubstTop t1' t2'
  in if isval ctx t1' && isvar ctx t2' && isvar ctx v'
    then v'
    else throw NoRuleApplies
evalBig _ctx t@TmAbs{} = t
evalBig _ctx _ =
  throw NoRuleApplies

-- ** helper functions
isvar :: Context -> Term -> Bool
isvar _ TmVar{} = True
isvar _ _ = False
