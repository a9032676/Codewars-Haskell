{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module AdditionCommutes
  ( plusCommutes ) where

import AdditionCommutes_Definitions
  ( Z, S
  , Natural(..), Equal(..)
  , (:+:)
  , reflexive, transitive)

suc :: Natural a -> Natural b -> Equal (S (a :+: b)) (a :+: S b)
suc  NumZ    b = EqlS $ reflexive b
suc (NumS a) b = EqlS $ suc a b

eq :: Natural a -> Equal (a :+: S Z) (S a)
eq  NumZ    = reflexive $ NumS NumZ
eq (NumS a) = EqlS $ eq a

-- This is the proof that the kata requires.
-- | a + b = b + a
plusCommutes :: Natural a -> Natural b -> Equal (a :+: b) (b :+: a)
plusCommutes  NumZ     NumZ    = EqlZ
plusCommutes (NumS a)  NumZ    = suc a NumZ
                                  `transitive` eq a
plusCommutes  NumZ    (NumS b) = EqlS $ plusCommutes NumZ b
plusCommutes (NumS a) (NumS b) = EqlS (plusCommutes a (NumS b))
                                  `transitive` EqlS (suc b a)