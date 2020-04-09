{-# LANGUAGE TypeOperators, TypeFamilies, GADTs, UndecidableInstances #-}

module TimesComm where

import TimesComm_Definitions
import Prelude hiding ((+), (*))

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

-- | You need this! Copy your solution from
-- https://www.codewars.com/kata/a-plus-b-plus-c-equals-a-plus-b-plus-c-prove-it/haskell
(+) :: Natural a -> Natural b -> Natural (a :+: b)
NumZ   + b = b
NumS a + b = NumS (a + b)

plusAssoc :: Natural a -> Natural b -> Natural c -> Equal (a :+: (b :+: c)) ((a :+: b) :+: c)
plusAssoc  NumZ    b c = reflexive $ b + c
plusAssoc (NumS a) b c = EqlS $ plusAssoc a b c

-- | You need this! Copy your solution from
-- https://www.codewars.com/kata/a-plus-b-equals-b-plus-a-prove-it/haskell
sucComm :: Natural a -> Natural b -> Equal (a :+: S b) (S (a :+: b))
sucComm  NumZ    b = EqlS $ reflexive b
sucComm (NumS a) b = EqlS $ sucComm a b

idr :: Natural b -> Equal (b :+: Z) b
idr  NumZ    = EqlZ
idr (NumS b) = EqlS $ idr b

-- This is the proof that the kata requires.
-- | a + b = b + a
plusComm :: Natural a -> Natural b -> Equal (a :+: b) (b :+: a)
plusComm a  NumZ    = idr a 
plusComm a (NumS b) = sucComm a b `transitive` (EqlS $ plusComm a b)

-- This is the proof that the kata requires.
-- | a * b = b * a
(*) :: Natural a -> Natural b -> Natural (a :*: b)
NumZ   * _ = NumZ
NumS a * b = b + (a * b)

-- This will also be helpful
zeroComm :: Natural a -> Equal Z (a :*: Z)
zeroComm  NumZ    = EqlZ
zeroComm (NumS a) = zeroComm a

timesComm :: Natural a -> Natural b -> Equal (a :*: b) (b :*: a)
timesComm a NumZ = symmetric $ zeroComm a
timesComm a b    = undefined