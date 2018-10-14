module Sec10.SimplyTyped where

import Data.Semigroup ((<>))
import Control.Exception

data NoRuleApplies = NoRuleApplies deriving (Show)

instance Exception NoRuleApplies

data Term
  = TmVar Info Int Int -- info, index, length of context
  | TmAbs Info String Ty Term -- info, hint name, type, term
  | TmApp Info Term Term -- info, term1, term2
  | TmTrue Info
  | TmFalse Info
  | TmIf Info Term Term Term
  deriving (Eq, Show)

data Ty 
  = TyArr Ty Ty -- * -> *
  | TyBool -- Bool
  deriving (Eq, Show)

data Info 
  = Info 
  deriving (Eq, Show)

type Context = [(Printable, Binding)]

data Binding 
  = NameBind
  | VarBind Ty -- assumption of typing
  deriving (Eq, Show)

type PP = Either String Printable

type Printable = String

-- * put

-- | put String notation
printtm :: Context -> Term -> PP
printtm ctx (TmVar fi x n) = if ctxlength ctx == n
  then Right (index2name fi ctx x)
  else Left "[bad index]"
printtm ctx (TmAbs _fi x _ty t1) = let (ctx', x') = pickfreshname ctx x in
  Right ("(λ " ++ x' ++ ". ") <> printtm ctx' t1 <> Right ")"
printtm ctx (TmApp _fi t1 t2) =
  Right "(" <> printtm ctx t1 <> printtm ctx t2 <> Right ")"
printtm _ctx (TmTrue _fi) = 
  Right "true"
printtm _ctx (TmFalse _fi) = 
  Right "false"
printtm ctx (TmIf _fi t1 t2 t3) = 
  Right "if " <> printtm ctx t1 
    <> Right " then " <> printtm ctx t2 
    <> Right " else " <> printtm ctx t3 
    

-- ** helper functions

ctxlength :: Context -> Int
ctxlength = subtract 1 . length

index2name :: Info -> Context -> Int -> Printable
index2name _fi ctx ix = fst (ctx !! ix)

pickfreshname :: Context -> Printable -> (Context, Printable)
pickfreshname ctx x = if x `elem` map fst ctx
  then pickfreshname ctx (x ++ "'")
  else ((x, NameBind) : ctx, x)

addbinding :: Context -> Printable -> Binding -> Context
addbinding ctx x bind = (x, bind) : ctx

getbinding :: Info -> Context -> Int -> Binding
getbinding fi ctx i = if length ctx > i 
  then snd $ ctx !! i
  else error' fi $ "Variable lookup failure: offset: "
    ++ show i ++ ", ctx size: " ++ show (length ctx)

error' :: Info -> String -> a
error' fi mes = error $ show fi ++ mes
    
-- * type check

typeof :: Context -> Term -> Ty
typeof ctx (TmVar fi i _) = getTypeFromContext fi ctx i
typeof ctx (TmAbs _fi x tyT1 t2) = let
  ctx' = addbinding ctx x (VarBind tyT1)
  tyT2 = typeof ctx' t2
  in TyArr tyT1 tyT2
typeof ctx (TmApp fi t1 t2) = let
  tyT1 = typeof ctx t1
  tyT2 = typeof ctx t2
  in case tyT1 of
    TyArr tyT11 tyT12 -> if (==) tyT2 tyT11 then tyT12
      else error' fi "parameter type mismatch"
    _ -> error' fi "arrow type expected"
typeof _ctx (TmTrue _fi) = TyBool
typeof _ctx (TmFalse _fi) = TyBool
typeof ctx (TmIf fi t1 t2 t3) = if (==) (typeof ctx t1) TyBool 
  then let tyT2 = typeof ctx t2 in
    if (==) tyT2 (typeof ctx t3) then tyT2
    else error' fi "arms of conditional have defferent types"
  else error' fi "guard of conditional not a boolean"
  
getTypeFromContext :: Info -> Context -> Int -> Ty
getTypeFromContext fi ctx i = 
  case getbinding fi ctx i of
    VarBind tyT -> tyT
    _ -> error' fi $ "getTypeFromContext: Wrong kind of binding for variable "
      ++ index2name fi ctx i

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
  walk c (TmAbs fi x ty t1) = TmAbs fi x ty (walk (c + 1) t1)
  walk c (TmApp fi t1 t2) = TmApp fi (walk c t1) (walk c t2)
  walk _c t@(TmTrue _) = t
  walk _c t@(TmFalse _) = t
  walk c (TmIf fi t1 t2 t3) = TmIf fi (walk c t1) (walk c t2) (walk c t3)

-- * evaluation

-- | 1-step evaluation
eval1 :: Context -> Term -> Term
-- E-AppAbs
eval1 ctx (TmApp _fi (TmAbs _fi' _x _ty t12) v2) | isval ctx v2 =
  termSubstTop v2 t12
-- E-App2
eval1 ctx (TmApp fi v1 t2) | isval ctx v1 =
  let t2' = eval1 ctx t2 in
  TmApp fi v1 t2'
-- E-App1
eval1 ctx (TmApp fi t1 t2) =
  let t1' = eval1 ctx t1 in
  TmApp fi t1' t2
-- E-IfTrue
eval1 _ctx (TmIf _ (TmTrue _) t2 _) =
  t2
-- E-IfFalse
eval1 _ctx (TmIf _ (TmFalse _) _ t3) =
  t3
-- E-If
eval1 ctx (TmIf fi t1 t2 t3) =
  let t1' = eval1 ctx t1 in
  TmIf fi t1' t2 t3  
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
isval _ TmTrue{} = True
isval _ TmFalse{} = True
isval _ _ = False
