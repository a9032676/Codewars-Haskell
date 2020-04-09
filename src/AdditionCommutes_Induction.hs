{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module AdditionCommutes_Induction
  ( plusCommutes ) where

import Prelude hiding (id, (+))

import AdditionCommutes_Definitions
  ( Z, S
  , Natural(..), Equal(..)
  , (:+:)
  , reflexive, transitive)

(+) :: Natural a -> Natural b -> Natural (a :+: b)
NumZ   + b = b
NumS a + b = NumS (a + b)

sucComm :: Natural a -> Natural b -> Equal (a :+: S b) (S (a :+: b))
sucComm  NumZ    b = EqlS $ reflexive b
sucComm (NumS a) b = EqlS $ sucComm a b

idr :: Natural b -> Equal (b :+: Z) b
idr  NumZ    = EqlZ
idr (NumS b) = EqlS $ idr b

-- This is the proof that the kata requires.
-- | a + b = b + a
plusCommutes :: Natural a -> Natural b -> Equal (a :+: b) (b :+: a)
plusCommutes a  NumZ    = idr a
plusCommutes a (NumS b) = sucComm a b `transitive` (EqlS $ plusCommutes a b)
