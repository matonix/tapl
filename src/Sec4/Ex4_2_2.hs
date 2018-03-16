module Sec4.Arith where

import Control.Exception

data NoRuleApplies = NoRuleApplies deriving (Show)

instance Exception NoRuleApplies

type Info = String

dummyInfo :: Info
dummyInfo = "dummy"

data Term =
    TmTrue Info
  | TmFalse Info
  | TmIf Info Term Term Term
  | TmZero Info
  | TmSucc Info Term
  | TmPred Info Term
  | TmIsZero Info Term
  deriving (Show)


isZero :: Term -> Bool
isZero (TmZero _) = True
isZero _ = False

isNumericVal :: Term -> Bool
isNumericVal (TmZero _) = True
isNumericVal (TmSucc _ t1) = isNumericVal t1
isNumericVal _ = False

isTrue :: Term -> Bool
isTrue (TmTrue _) = True
isTrue _ = False

isFalse :: Term -> Bool
isFalse (TmFalse _) = True
isFalse _ = False

isVal :: Term -> Bool
isVal t
  | isTrue t = True
  | isFalse t = True
  | isNumericVal t = True
  | otherwise = False

evalBig :: Term -> Term
evalBig (TmIf _ t1 t2 t3)
  | isTrue (evalBig t1) && isVal (evalBig t2) = evalBig t2
  | isFalse (evalBig t1) && isVal (evalBig t3) = evalBig t3
evalBig (TmSucc fi t1)
  | isNumericVal (evalBig t1) = TmSucc fi (evalBig t1)
evalBig (TmPred _ t1)
  | isZero (evalBig t1) = TmZero dummyInfo
  | isNumericVal (evalBig t1) = evalBig t1
evalBig (TmIsZero _ t1)
  | isZero (evalBig t1) = TmTrue dummyInfo
  | isNumericVal (evalBig t1) = TmFalse dummyInfo
evalBig _ =
  throw NoRuleApplies

eval :: Term -> IO Term
eval t = do
  t' <- try (evaluate (evalBig t))
  case t' of
    Left NoRuleApplies -> return t -- buggy?
    Right t'' -> eval t''

-- Ex 4.2.1 [**]
-- evalBig :: Term -> Maybe Term とし、
-- 推論規則にマッチする場合は Just で評価後の項を包んで返し、
-- マッチしない場合は NoRuleApplies の代わりに Nothing を返す。
