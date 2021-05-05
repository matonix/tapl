{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Revisited.Bool where

import Control.Exception.Safe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Revisited.Bool.Syntax

data EvalException = NoRuleApplies deriving (Show)

instance Exception EvalException

-- | term example
-- >>> ex
-- TmIf TmTrue TmTrue TmFalse
ex :: Term
ex = [bool|if true then true else false|]

-- >>> ex2
-- TmIf TmTrue TmTrue (TmIf TmFalse TmFalse TmFalse)
ex2 :: Term
ex2 = [bool|if false then true else (if false then false else false)|]

-- >>> ex3
-- TmIf (TmIf (TmIf TmTrue TmFalse TmFalse) TmTrue TmTrue) TmFalse TmFalse
ex3 :: Term
ex3 = [bool|if (if (if true then false else false) then true else true) then false else false|]

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

-- >>> traceEval1 @IO ex2 Empty
-- (TmIf TmFalse TmFalse TmFalse,Trace [Empty] "E-IfFalse" (TmIf TmFalse TmTrue (TmIf TmFalse TmFalse TmFalse)))

-- >>> traceEval @IO ex2 Empty
-- (TmFalse,Trace [Trace [Empty] "E-IfFalse" (TmIf TmFalse TmTrue (TmIf TmFalse TmFalse TmFalse))] "E-IfFalse" (TmIf TmFalse TmFalse TmFalse))

-- print系の処理が hls の doctest では表示できなくなっている模様（error 経由ならできる）
-- https://github.com/haskell/haskell-language-server/blob/master/plugins/hls-eval-plugin/README.md
-- >>> result <- traceEval1 @IO ex3 Empty
-- >>> prettyPrint = error . T.unpack . pprint . snd :: (Term, Trace) -> IO String
-- >>> prettyPrint result
-- --------------------------------------E-IfTrue
-- TmIf TmTrue TmFalse TmFalse → TmFalse
-- ------------------------------------------------------------------------------E-If
-- TmIf (TmIf TmTrue TmFalse TmFalse) TmTrue TmTrue → TmIf TmFalse TmTrue TmTrue
-- ----------------------------------------------------------------------------------------------------------------------------E-If
-- TmIf (TmIf (TmIf TmTrue TmFalse TmFalse) TmTrue TmTrue) TmFalse TmFalse → TmIf (TmIf TmFalse TmTrue TmTrue) TmFalse TmFalse

-- TODO: Trace の順番を直す
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

pprint :: Trace -> Text
pprint tr = T.unlines $ pprint' tr

pprint' :: Trace -> [Text]
pprint' (Trace traces rule t1 t2) =
  let t1Text = T.pack (show t1)
      t2Text = T.pack (show t2)
   in foldl1 merge (map pprint' traces)
        <> [ T.replicate (T.length t1Text + T.length t2Text + 4) "-" <> rule,
             t1Text <> " → " <> t2Text
           ]
  where
    merge :: [Text] -> [Text] -> [Text]
    merge t1' t2' = let width = succ $ maximum $ map T.length t1' in zipWith T.append (map (T.justifyLeft width ' ') t1') t2'
pprint' Empty = mempty

printTrace :: (Term, Trace) -> IO ()
printTrace = T.putStrLn . pprint . snd
