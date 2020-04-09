{-# LANGUAGE NoImplicitPrelude, GADTs , DataKinds, TypeFamilies, TypeOperators, RankNTypes #-}

module Singletons where

import Prelude hiding (drop, take, head, tail, index, zipWith, replicate, map, (++))

data Vec a n where
  VNil :: Vec a Zero
  VCons :: a -> Vec a n -> Vec a (Succ n)

-- promoted to type level by data kinds
data Nat = Zero | Succ Nat

data SNat a where
  SZero :: SNat Zero
  SSucc :: SNat a -> SNat (Succ a)

type family (a :: Nat) :< (b :: Nat) :: Bool
type instance m      :< Zero   = False
type instance Zero   :< Succ n = True
type instance Succ m :< Succ n = m :< n

type family (Add (a :: Nat) (b :: Nat)) :: Nat
type instance Add  Zero    b = b
type instance Add (Succ a) b = Succ (Add a b)

type family (Sub (a :: Nat) (b :: Nat)) :: Nat
type instance Sub  m        Zero    = m
type instance Sub  Zero     n       = Zero
type instance Sub (Succ m) (Succ n) = Sub m n

type family (Min (a :: Nat) (b :: Nat)) :: Nat
type instance Min  Zero     n       = Zero
type instance Min  m        Zero    = Zero
type instance Min (Succ m) (Succ n) = Succ (Min m n)

map :: (a -> b) -> Vec a n -> Vec b n
map f  VNil        = VNil
map f (VCons x xs) = VCons (f x) $ map f xs

index :: ((a :< b) ~ True) => SNat a -> Vec s b -> s
index  SZero    (VCons v _ ) = v
index (SSucc n) (VCons _ xs) = index n xs

replicate :: s -> SNat a -> Vec s a
replicate _  SZero    = VNil
replicate s (SSucc n) = VCons s $ replicate s n

-- -- Both vectors must be of equal length
zipWith :: (a -> b -> c) -> Vec a n -> Vec b n -> Vec c n
zipWith _  VNil         _            = VNil
zipWith f (VCons a xs) (VCons b xs') = VCons (f a b) $ zipWith f xs xs'

(++) :: Vec a m -> Vec a n -> Vec a (Add m n)
VNil         ++ b = b
(VCons a xs) ++ b = VCons a $ xs ++ b

-- -- The semantics should match that of take for normal lists.
take :: SNat m -> Vec a n -> Vec a (Min m n)
take  SZero     _           = VNil
take  _         VNil        = VNil
take (SSucc n) (VCons a xs) = VCons a $ take n xs

-- -- The semantics should match that of drop for normal lists.
drop :: SNat m -> Vec a n -> Vec a (Sub n m)
drop  SZero     b           = b
drop  _         VNil        = VNil
drop (SSucc n) (VCons _ xs) = drop n xs

head :: Vec a n -> a
head  VNil       = error "empty list"
head (VCons a _) = a

tail :: Vec a (Succ n) -> Vec a n
tail (VCons _ xs) = xs
