{-# LANGUAGE TypeOperators, TypeFamilies, GADTs #-}

module AdditionAssoc where

import Prelude hiding ((+))

-- | The natural numbers, encoded in types.
data Z
data S n

-- | Predicate describing natural numbers.
-- | This allows us to reason with `Nat`s.
data Natural :: * -> * where
  NumZ :: Natural Z
  NumS :: Natural n -> Natural (S n)

-- | Predicate describing equality of natural numbers.
data Equal :: * -> * -> * where
  EqlZ :: Equal Z Z
  EqlS :: Equal n m -> Equal (S n) (S m)

-- | Peano definition of addition.
type family (:+:) (n :: *) (m :: *) :: *
type instance Z :+: m = m
type instance S n :+: m = S (n :+: m)

-- | For any n, n = n.
reflexive :: Natural n -> Equal n n
reflexive NumZ = EqlZ
reflexive (NumS n) = EqlS $ reflexive n

-- | if a = b, then b = a.
symmetric :: Equal a b -> Equal b a
symmetric EqlZ = EqlZ
symmetric (EqlS eq) = EqlS $ symmetric eq

(+) :: Natural a -> Natural b -> Natural (a :+: b)
NumZ   + b = b
NumS a + b = NumS (a + b)

-- This is the proof that the kata requires.
-- | a + (b + c) = (a + b) + c
-- Z + (b + c)
-- b + c
-- (Z + b) + c
plusAssoc :: Natural a -> Natural b -> Natural c -> Equal (a :+: (b :+: c)) ((a :+: b) :+: c)
plusAssoc  NumZ    b c = reflexive $ b + c
plusAssoc (NumS a) b c = EqlS $ plusAssoc a b c
-- plusAssoc NumZ NumZ NumZ     = EqlZ
-- plusAssoc NumZ NumZ (NumS c) = EqlS $ plusAssoc NumZ NumZ c
-- plusAssoc NumZ (NumS b) c    = EqlS $ plusAssoc NumZ b c
-- plusAssoc (NumS a) b c       = EqlS $ plusAssoc a b c