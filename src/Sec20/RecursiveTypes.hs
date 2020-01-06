{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Sec20.RecursiveTypes where

-- 再帰演算子は使えないのでHaskellの再帰的定義の機能をそのまま使う。
-- ADTの機能を制限し、直和をシミュレートする。
-- パターンマッチは直和のラベルの分離の目的にのみ使用可能とする。
-- レコードも Lens.Micro.TH で後置記法によるアクセスが可能となるが、doctest で面倒なことになるのでやめた。

import           Data.Function                  ( fix )
import           Lens.Micro

type Nat = Int
data Unit = Unit
    deriving Show

data NatList = Nil Unit | Cons (Nat, NatList)
    deriving Show

nil :: NatList
nil = Nil Unit

cons :: Nat -> NatList -> NatList
cons n l = Cons (n, l)

isnil :: NatList -> Bool
isnil l = case l of
    Nil _  -> True
    Cons _ -> False

hd :: NatList -> Nat
hd l = case l of
    Nil _  -> 0
    Cons p -> p ^. _1

tl :: NatList -> NatList
tl l = case l of
    Nil _  -> l
    Cons p -> p ^. _2

sumlist :: NatList -> Nat
sumlist = fix (\s l -> if isnil l then 0 else (+) (hd l) (s (tl l)))

-- |
-- >>> sumlist (cons 2 (cons 3 (cons 5 nil)))
-- 10

-- * 20.1.1

data NatTree = Leaf Unit | Branch (Nat, NatTree, NatTree)
    deriving Show

leaf :: NatTree
leaf = Leaf Unit

-- |
-- Leftist Postorder Tree
-- 二分木というか、ストアが2つあるスタックかも。
grow :: Nat -> NatTree -> NatTree
grow n t = case t of
    Leaf _ -> Branch (n, leaf, leaf)
    Branch p -> case p ^. _2 of
        Leaf _ -> Branch (n, Branch p, p ^. _3)
        Branch _ -> case p ^. _3 of
            Leaf _ -> Branch (n, p ^. _2, Branch (p ^. _1, leaf, leaf))
            Branch _ -> Branch (n, Branch p, leaf)

-- |
-- >>> grow 0 (Leaf Unit)
-- Branch (0,Leaf Unit,Leaf Unit)
-- >>> grow 1 (Branch (0,Leaf Unit,Leaf Unit))
-- Branch (1,Branch (0,Leaf Unit,Leaf Unit),Leaf Unit)
-- >>> grow 2 (Branch (1,Branch (0,Leaf Unit,Leaf Unit),Leaf Unit))
-- Branch (2,Branch (0,Leaf Unit,Leaf Unit),Branch (1,Leaf Unit,Leaf Unit))
-- >>> grow 3 (Branch (2,Branch (0,Leaf Unit,Leaf Unit),Branch (1,Leaf Unit,Leaf Unit)))
-- Branch (3,Branch (2,Branch (0,Leaf Unit,Leaf Unit),Branch (1,Leaf Unit,Leaf Unit)),Leaf Unit)

prune :: NatTree -> (Nat, NatTree)
prune t = case t of
    Leaf _ -> error "leaf"
    Branch p -> case p ^. _3 of
        Leaf _ -> case p ^. _2 of
            Leaf _ -> (p ^. _1, leaf)
            Branch _ -> (p ^. _1, p ^. _2)
        Branch p' -> (p ^. _1, Branch (p' ^. _1, p ^. _2, leaf))

-- |
-- >>> prune (Branch (0,Leaf Unit,Leaf Unit))
-- (0,Leaf Unit)
-- >>> prune (Branch (1,Branch (0,Leaf Unit,Leaf Unit),Leaf Unit))
-- (1,Branch (0,Leaf Unit,Leaf Unit))
-- >>> prune (Branch (2,Branch (0,Leaf Unit,Leaf Unit),Branch (1,Leaf Unit,Leaf Unit)))
-- (2,Branch (1,Branch (0,Leaf Unit,Leaf Unit),Leaf Unit))
-- >>> prune (Branch (3,Branch (2,Branch (0,Leaf Unit,Leaf Unit),Branch (1,Leaf Unit,Leaf Unit)),Leaf Unit))
-- (3,Branch (2,Branch (0,Leaf Unit,Leaf Unit),Branch (1,Leaf Unit,Leaf Unit)))

-- 木の検査ってなんだろう

-- helper function
con :: NatList -> NatList -> NatList
con = fix (\f l1 l2 -> case l1 of
    Nil _ -> l2
    Cons p -> cons (p ^. _1) (f (p ^. _2) l2))

tolist :: NatTree -> NatList
tolist = fix (\f t -> case t of
    Leaf _ -> nil
    Branch p -> con (con (f (p ^. _2)) (f (p ^. _3))) (cons (p ^. _1) nil))

-- |
-- >>> tolist (Branch (0,Leaf Unit,Leaf Unit))
-- Cons (0,Nil Unit)
-- >>> tolist (Branch (1,Branch (0,Leaf Unit,Leaf Unit),Leaf Unit))
-- Cons (0,Cons (1,Nil Unit))
-- >>> tolist (Branch (2,Branch (0,Leaf Unit,Leaf Unit),Branch (1,Leaf Unit,Leaf Unit)))
-- Cons (0,Cons (1,Cons (2,Nil Unit)))
-- >>> tolist (Branch (3,Branch (2,Branch (0,Leaf Unit,Leaf Unit),Branch (1,Leaf Unit,Leaf Unit)),Leaf Unit))
-- Cons (0,Cons (1,Cons (2,Cons (3,Nil Unit))))

-- Hungry function

newtype Hungry = Hungry { unhungry :: Nat -> Hungry }

h :: Hungry
h = fix (\f -> Hungry (\_ -> f))

-- unhungry (unhungry h 0) 1 :: Hungry

-- Stream 

newtype Stream = Stream { next :: Unit -> (Nat, Stream) }

hdS :: Stream -> Nat
hdS s = next s Unit ^. _1

tlS :: Stream -> Stream
tlS s = next s Unit ^. _2

upfrom0 :: Stream
upfrom0 = fix (\f n -> Stream (\_ -> (n, f (succ n)))) 0

-- |
-- >>> hdS (tlS (tlS (tlS upfrom0)))
-- 3

-- * 20.1.2

fibo :: Stream
fibo = fix (\f n m -> Stream (\_ -> (n, f m (n + m)))) 1 1

-- |
-- >>> hdS fibo
-- 1
-- >>> hdS (tlS fibo)
-- 1
-- >>> hdS (tlS (tlS fibo))
-- 2
-- >>> hdS (tlS (tlS (tlS fibo)))
-- 3
-- >>> hdS (tlS (tlS (tlS (tlS fibo))))
-- 5
-- >>> hdS (tlS (tlS (tlS (tlS (tlS fibo)))))
-- 8

-- * 20.1.3

data Counter = Counter 
    { get :: Nat
    , inc :: Unit -> Counter 
    , dec :: Unit -> Counter
    , backup :: Unit -> Counter
    , reset :: Unit -> Counter
    }

data CounterRep = CounterRep 
    { x :: Nat 
    , b :: Nat
    }

c :: Counter
c = let create = fix (\f s -> Counter
            { get = x s
            , inc = \_ -> f (CounterRep {x = succ (x s), b = b s})
            , dec = \_ -> f (CounterRep {x = pred (x s), b = b s})
            , backup = \_ -> f (CounterRep {x = x s, b = x s})
            , reset = \_ -> f (CounterRep {x = b s, b = b s})
            })
    in create (CounterRep {x = 0, b = 0})

-- |
-- >>> let c1 = (inc c) Unit
-- >>> let c2 = (inc c1) Unit
-- >>> get c2
-- 2
-- >>> let c3 = (backup c2) Unit
-- >>> let c4 = (inc c3) Unit
-- >>> get c4
-- 3
-- >>> let c5 = (reset c4) Unit
-- >>> get c5
-- 2

-- Untyped λ-calculus revisited
-- Extensible Variant でまとめればよかった…
-- 本当に発散されても困るので `divergeD` の代わりに `error` を使う。

data D = Nat Nat
       | Fn (D -> D)

lam :: (D -> D) -> D
lam f = Fn f

ap :: D -> D -> D
ap f a = case f of
    Fn f -> f a
    Nat _ -> error "ap"

-- 何故かビルド中にコンパイラが死ぬのでコメントアウトする。

-- fixD :: D
-- fixD = lam (\f -> 
--         ap (lam (\x -> ap f (ap x x)))
--            (lam (\x -> ap f (ap x x))))

-- divergeD :: Unit -> D
-- divergeD _ = ap fixD (Fn id)

suc :: D -> D
suc f = case f of
    Nat n -> Nat (succ n)
    _ -> error "suc"

zro :: D
zro = Nat 0

-- * 20.1.4

-- data D4 = Nat4 Nat
--         | Fn4 (D4 -> D4)
--         | Bool Bool
--         | If Bool D4 D4

-- 誤りだったので書き直し

data D4 = Nat4 Nat
        | Fn4 (D4 -> D4)
        | Bool Bool

instance Show D4 where
    show (Nat4 n) = show n
    show (Fn4 _) = "<fn>"
    show (Bool b) = show b

-- |
-- >>> ifd fls one4 zro4
-- 0
-- >>> ifd fls one4 fls
-- False
ifd :: D4 -> D4 -> D4 -> D4
ifd b t e = case b of
    Bool b' -> if b' then t else e
    _ -> error "ifd"

tru :: D4
tru = Bool True

fls :: D4
fls = Bool False

one4 :: D4
one4 = Nat4 1

zro4 :: D4
zro4 = Nat4 0

-- * 20.1.5

data D5 = Nat5 Nat
        | Fn5 (D5 -> D5)
        | Rcd (Nat -> D5)

instance Show D5 where
    show (Nat5 n) = show n
    show (Fn5 _) = "<fn>"
    show (Rcd _) = "<rcd>"

rcd :: (Nat -> D5) -> D5
rcd f = Rcd f

proj :: D5 -> Nat -> D5
proj f n = case f of
    Rcd r -> r n
    _ -> error "proj"

-- |
-- >>> let r1 = rcd (\n -> case n of { 0 -> Nat5 1 } )
-- >>> proj r1 0
-- 1

-- * 20.2.1

newtype T = Fold { unfold :: T -> T }

fixT :: (T -> T) -> T
fixT = \(f :: T -> T) -> 
    (\(x :: T) -> f (unfold x x)) 
    (Fold (\(x :: T) -> f (unfold x x)))

{-# ANN fixT "HLint: ignore Redundant lambda" #-}
