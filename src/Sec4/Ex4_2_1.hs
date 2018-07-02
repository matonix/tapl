module Sec4.Ex4_2_1 where

import Sec4.Arith

-- Ex 4.2.1 [**]
-- eval1Maybe :: Term -> Maybe Term とし、
-- 推論規則にマッチする場合は Just で評価後の項を包んで返し、
-- マッチしない場合は NoRuleApplies の代わりに Nothing を返す。

eval1Maybe :: Term -> Maybe Term
eval1Maybe (TmIf _ (TmTrue _) t2 _) =
  Just t2
eval1Maybe (TmIf _ (TmFalse _) _ t3) =
  Just t3
eval1Maybe (TmIf fi t1 t2 t3) =
  let t1' = eval1Maybe t1 in
  fmap (\t -> TmIf fi t t2 t3) t1'
eval1Maybe (TmSucc fi t1) =
  let t1' = eval1Maybe t1 in
  fmap (TmSucc fi) t1'
eval1Maybe (TmPred _ (TmZero _)) =
  Just (TmZero dummyInfo)
eval1Maybe (TmPred _ (TmSucc _ nv1)) | isNumericVal nv1 =
  Just nv1
eval1Maybe (TmPred fi t1) =
  let t1' = eval1Maybe t1 in
  fmap (TmIsZero fi) t1'
eval1Maybe (TmIsZero _ (TmZero _)) =
  Just (TmTrue dummyInfo)
eval1Maybe (TmIsZero _ (TmSucc _ nv1)) | isNumericVal nv1 =
  Just (TmFalse dummyInfo)
eval1Maybe (TmIsZero fi t1) =
  let t1' = eval1Maybe t1 in
  fmap (TmIsZero fi) t1'
eval1Maybe _ =
  Nothing

eval' :: Term -> Term
eval' t =
  let t' = eval1Maybe t in
  maybe t eval' t'
