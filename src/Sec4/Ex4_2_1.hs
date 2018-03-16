module Sec4.Ex4_2_1 where

-- Ex 4.2.1 [**]
-- eval1 :: Term -> Maybe Term とし、
-- 推論規則にマッチする場合は Just で評価後の項を包んで返し、
-- マッチしない場合は NoRuleApplies の代わりに Nothing を返す。

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

eval1 :: Term -> Maybe Term
eval1 (TmIf _ (TmTrue _) t2 _) =
  Just t2
eval1 (TmIf _ (TmFalse _) _ t3) =
  Just t3
eval1 (TmIf fi t1 t2 t3) =
  let t1' = eval1 t1 in
  fmap (\t -> TmIf fi t t2 t3) t1'
eval1 (TmSucc fi t1) =
  let t1' = eval1 t1 in
  fmap (TmSucc fi) t1'
eval1 (TmPred _ (TmZero _)) =
  Just (TmZero dummyInfo)
eval1 (TmPred _ (TmSucc _ nv1)) | isNumericVal nv1 =
  Just nv1
eval1 (TmPred fi t1) =
  let t1' = eval1 t1 in
  fmap (TmIsZero fi) t1'
eval1 (TmIsZero _ (TmZero _)) =
  Just (TmTrue dummyInfo)
eval1 (TmIsZero _ (TmSucc _ nv1)) | isNumericVal nv1 =
  Just (TmFalse dummyInfo)
eval1 (TmIsZero fi t1) =
  let t1' = eval1 t1 in
  fmap (TmIsZero fi) t1'
eval1 _ =
  Nothing

eval :: Term -> Term
eval t =
  let t' = eval1 t in
  maybe t eval t'
