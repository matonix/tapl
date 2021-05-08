{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Revisited.NB.Semantics where

import Control.Exception.Safe
import Data.Text (Text)
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy as TL
import Revisited.NB.Syntax

data EvalException = NoRuleApplies deriving (Show)

instance Exception EvalException

-- | term example

-- >>> ex
-- TmPred (TmSucc (TmPred TmZero))
ex :: Term
ex = [nb|pred (succ (pred 0))|]

-- >>> render <$> eval1 @IO ex
-- "pred (succ 0)"

-- >>> render <$> eval @IO ex
-- "0"

eval1 :: MonadThrow m => Term -> m Term
eval1 (TmIf TmTrue t2 _t3) = return t2
eval1 (TmIf TmFalse _t2 t3) = return t3
eval1 (TmIf t1 t2 t3) = do
  t1' <- eval1 t1
  return $ TmIf t1' t2 t3
eval1 (TmSucc t1) = do
  t1' <- eval1 t1
  return $ TmSucc t1'
eval1 (TmPred TmZero) = return TmZero
eval1 (TmPred (TmSucc nv1)) | isNumericVal nv1 = return nv1
eval1 (TmPred t1) = do
  t1' <- eval1 t1
  return $ TmPred t1'
eval1 (TmIszero TmZero) = return TmTrue
eval1 (TmIszero (TmSucc nv1)) | isNumericVal nv1 = return TmFalse
eval1 (TmIszero t1) = do
  t1' <- eval1 t1
  return $ TmIszero t1'
eval1 _ =
  throw NoRuleApplies

isNumericVal :: Term -> Bool
isNumericVal TmZero = True
isNumericVal (TmSucc t1) = isNumericVal t1
isNumericVal _ = False

isVal :: Term -> Bool
isVal TmTrue = True
isVal TmFalse = True
isVal t | isNumericVal t = True
isVal _ = False

eval :: MonadCatch m => Term -> m Term
eval t = do
  e <- try $ eval1 t
  case e of
    Left NoRuleApplies -> return t
    Right t' -> eval t'

-- | derivation tree
data Trace = Trace [Trace] Text Term Term | Empty -- Rose tree
  deriving (Show)

traceEval1 :: MonadThrow m => Term -> Trace -> m (Term, Trace)
traceEval1 t@(TmIf TmTrue t2 _t3) tr = return (t2, Trace [tr] "E-IfTrue" t t2)
traceEval1 t@(TmIf TmFalse _t2 t3) tr = return (t3, Trace [tr] "E-IfFalse" t t3)
traceEval1 t@(TmIf t1 t2 t3) tr = do
  (t1', tr') <- traceEval1 t1 tr
  let t' = TmIf t1' t2 t3
  return (t', Trace [tr'] "E-If" t t')
traceEval1 t@(TmSucc t1) tr = do
  (t1', tr') <- traceEval1 t1 tr
  let t' = TmSucc t1'
  return (t', Trace [tr'] "E-Succ" t t')
traceEval1 t@(TmPred TmZero) tr = return (TmZero, Trace [tr] "E-PredZero" t TmZero)
traceEval1 t@(TmPred (TmSucc nv1)) tr | isNumericVal nv1 = return (nv1, Trace [tr] "E-PredSucc" t nv1)
traceEval1 t@(TmPred t1) tr = do
  (t1', tr') <- traceEval1 t1 tr
  let t' = TmPred t1'
  return (t', Trace [tr'] "E-Pred" t t')
traceEval1 t@(TmIszero TmZero) tr = return (TmTrue, Trace [tr] "E-IszeroZero" t TmTrue)
traceEval1 t@(TmIszero (TmSucc nv1)) tr | isNumericVal nv1 = return (TmFalse, Trace [tr] "E-IszeroSucc" t TmFalse)
traceEval1 t@(TmIszero t1) tr = do
  (t1', tr') <- traceEval1 t1 tr
  let t' = TmIszero t1'
  return (t', Trace [tr'] "E-Iszero" t t')
traceEval1 _ _ =
  throw NoRuleApplies

traceEval :: MonadCatch m => Term -> Trace -> m (Term, Trace)
traceEval t tr = do
  e <- try $ traceEval1 t tr
  case e of
    Left NoRuleApplies -> return (t, tr)
    Right (t', tr') -> traceEval t' tr'

renderTrace :: Trace -> TL.Text
renderTrace tr = TL.unlines $ renderTrace' tr

renderTrace' :: Trace -> [TL.Text]
renderTrace' (Trace traces rule t1 t2) =
  let t1Text = render t1
      t2Text = render t2
   in foldl1 merge (map renderTrace' traces)
        <> [ TL.replicate (TL.length t1Text + TL.length t2Text + 4) "-" <> TL.fromStrict rule,
             t1Text <> " → " <> t2Text
           ]
  where
    merge :: [TL.Text] -> [TL.Text] -> [TL.Text]
    merge t1' t2' = let width = succ $ maximum $ map TL.length t1' in zipWith TL.append (map (TL.justifyLeft width ' ') t1') t2'
renderTrace' Empty = mempty

printTrace :: (Term, Trace) -> IO ()
printTrace = TL.putStrLn . renderTrace . snd

-- >>> traceEval1 @IO ex Empty
-- (TmPred (TmSucc TmZero),Trace [Trace [Trace [Empty] "E-PredZero" (TmPred TmZero) TmZero] "E-Succ" (TmSucc (TmPred TmZero)) (TmSucc TmZero)] "E-Pred" (TmPred (TmSucc (TmPred TmZero))) (TmPred (TmSucc TmZero)))

-- >>> result <- traceEval1 @IO ex Empty
-- >>> prettyPrint = error . TL.unpack . renderTrace . snd :: (Term, Trace) -> IO String
-- >>> prettyPrint result
-- -----------E-PredZero
-- pred 0 → 0
-- -----------------------E-Succ
-- succ (pred 0) → succ 0
-- -------------------------------------E-Pred
-- pred (succ (pred 0)) → pred (succ 0)
