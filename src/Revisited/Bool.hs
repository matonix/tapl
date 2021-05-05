{-# LANGUAGE QuasiQuotes #-}

module Revisited.Bool where

import Revisited.Bool.Syntax

-- | term example
-- >>> ex
-- TmIf TmTrue TmTrue TmFalse
ex :: Term
ex = [bool|if true then true else false|]

-- >>> ex2
-- TmIf TmTrue TmTrue (TmIf TmFalse TmFalse TmFalse)
ex2 :: Term
ex2 = [bool|if false then true else (if false then false else false)|]

-- * small-step evaluating

-- >>> eval1 ex
-- TmTrue
-- >>> eval1 ex2
-- TmIf TmFalse TmFalse TmFalse
-- >>> eval ex2
-- TmFalse

eval1 :: Term -> Term
eval1 (TmIf TmTrue t2 _t3) = t2
eval1 (TmIf TmFalse _t2 t3) = t3
eval1 (TmIf t1 t2 t3) =
  let t1' = eval1 t1
   in TmIf t1' t2 t3
eval1 _ =
  error "No rule applies."

isVal :: Term -> Bool
isVal TmTrue = True
isVal TmFalse = True
isVal _ = False

eval :: Term -> Term
eval t = if isVal t
  then t
  else eval $ eval1 t
