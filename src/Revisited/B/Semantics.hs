{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Revisited.B.Semantics where

import Control.Exception.Safe
import Data.Text (Text)
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy as TL
import Revisited.B.Syntax

data EvalException = NoRuleApplies deriving (Show)

instance Exception EvalException

-- | term example

-- >>> ex
-- TmIf TmTrue TmTrue TmFalse
-- >>> render ex
-- "if true then true else false"
ex :: Term
ex = [b|if true then true else false|]


-- >>> ex2
-- TmIf TmFalse TmTrue (TmIf TmFalse TmFalse TmFalse)
-- >>> render ex2
-- "if false then true else (if false then false else false)"
ex2 :: Term
ex2 = [b|if false then true else (if false then false else false)|]

-- >>> ex3
-- TmIf (TmIf (TmIf TmTrue TmFalse TmFalse) TmTrue TmTrue) TmFalse TmFalse
-- >>> render ex3
-- "if (if (if true then false else false) then true else true) then false else false"
ex3 :: Term
ex3 = [b|if (if (if true then false else false) then true else true) then false else false|]

-- * small-step evaluating

-- >>> eval1 @IO ex
-- TmTrue
-- >>> eval1 @IO ex2
-- TmIf TmFalse TmFalse TmFalse
-- >>> eval @IO ex2
-- TmFalse

eval1 :: MonadThrow m => Term -> m Term
eval1 (TmIf TmTrue t2 _t3) = return t2
eval1 (TmIf TmFalse _t2 t3) = return t3
eval1 (TmIf t1 t2 t3) = do
  t1' <- eval1 t1
  return $ TmIf t1' t2 t3
eval1 _ =
  throw NoRuleApplies

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

-- >>> traceEval1 @IO ex2 Empty
-- (TmIf TmFalse TmFalse TmFalse,Trace [Empty] "E-IfFalse" (TmIf TmFalse TmTrue (TmIf TmFalse TmFalse TmFalse)) (TmIf TmFalse TmFalse TmFalse))

-- >>> traceEval @IO ex2 Empty
-- (TmFalse,Trace [Trace [Empty] "E-IfFalse" (TmIf TmFalse TmTrue (TmIf TmFalse TmFalse TmFalse))] "E-IfFalse" (TmIf TmFalse TmFalse TmFalse))

-- print系の処理が hls の doctest では表示できなくなっている模様（error 経由ならできる）
-- https://github.com/haskell/haskell-language-server/blob/master/plugins/hls-eval-plugin/README.md
-- >>> result <- traceEval1 @IO ex3 Empty
-- >>> prettyPrint = error . TL.unpack . renderTrace . snd :: (Term, Trace) -> IO String
-- >>> prettyPrint result
-- --------------------------------------E-IfTrue
-- if true then false else false → false
-- --------------------------------------------------------------------------------------E-If
-- if (if true then false else false) then true else true → if false then true else true
-- --------------------------------------------------------------------------------------------------------------------------------------------E-If
-- if (if (if true then false else false) then true else true) then false else false → if (if false then true else true) then false else false
