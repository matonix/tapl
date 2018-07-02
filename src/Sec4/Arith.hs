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

isNumericVal :: Term -> Bool
isNumericVal (TmZero _) = True
isNumericVal (TmSucc _ t1) = isNumericVal t1
isNumericVal _ = False

isVal :: Term -> Bool
isVal (TmTrue _) = True
isVal (TmFalse _) = True
isVal t | isNumericVal t = True
isVal _ = False

eval1 :: Term -> Term
eval1 (TmIf _ (TmTrue _) t2 _) =
  t2
eval1 (TmIf _ (TmFalse _) _ t3) =
  t3
eval1 (TmIf fi t1 t2 t3) =
  let t1' = eval1 t1 in
  TmIf fi t1' t2 t3
eval1 (TmSucc fi t1) =
  let t1' = eval1 t1 in
  TmSucc fi t1'
eval1 (TmPred _ (TmZero _)) =
  TmZero dummyInfo
eval1 (TmPred _ (TmSucc _ nv1)) | isNumericVal nv1 =
  nv1
eval1 (TmPred fi t1) =
  let t1' = eval1 t1 in
  TmPred fi t1'
eval1 (TmIsZero _ (TmZero _)) =
  TmTrue dummyInfo
eval1 (TmIsZero _ (TmSucc _ nv1)) | isNumericVal nv1 =
  TmFalse dummyInfo
eval1 (TmIsZero fi t1) =
  let t1' = eval1 t1 in
  TmIsZero fi t1'
eval1 _ =
  throw NoRuleApplies

eval :: Term -> IO Term
eval t = do
  t' <- try (evaluate (eval1 t))
  case t' of
    Left NoRuleApplies -> return t -- buggy?
    Right t'' -> eval t''
