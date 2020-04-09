{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
module AdditionCommutes_Definitions where

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

-- These are some lemmas that may be helpful.
-- They will *not* be tested, so rename them
-- if you so desire. Good luck!

-- | For any n, n = n.
reflexive :: Natural n -> Equal n n
reflexive  NumZ    = EqlZ
reflexive (NumS n) = EqlS $ reflexive n

-- | if a = b, then b = a.
symmetric :: Equal a b -> Equal b a
symmetric  EqlZ     = EqlZ
symmetric (EqlS ab) = EqlS $ symmetric ab

-- | if a = b and b = c, then a = c.
transitive :: Equal a b -> Equal b c -> Equal a c
transitive  EqlZ      EqlZ     = EqlZ
transitive (EqlS ab) (EqlS bc) = EqlS $ transitive ab bc