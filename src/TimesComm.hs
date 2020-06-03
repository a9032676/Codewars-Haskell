{-# LANGUAGE TypeOperators, TypeFamilies, GADTs, UndecidableInstances #-}

module TimesComm where

import TimesComm_Definitions
import Prelude hiding ((>>), (+), (*))

-- | For any n, n = n.
refl :: Natural n -> Equal n n
refl  NumZ    = EqlZ
refl (NumS n) = EqlS $ refl n

-- | if a = b, then b = a.
sym :: Equal a b -> Equal b a
sym  EqlZ     = EqlZ
sym (EqlS ab) = EqlS $ sym ab

-- | if a = b and b = c, then a = c.
(>>) :: Equal a b -> Equal b c -> Equal a c
(>>)  EqlZ      EqlZ     = EqlZ
(>>) (EqlS ab) (EqlS bc) = EqlS (ab >> bc)

-- | You need this! Copy your solution from
-- https://www.codewars.com/kata/a-plus-b-plus-c-equals-a-plus-b-plus-c-prove-it/haskell
(+) :: Natural a -> Natural b -> Natural (a :+: b)
NumZ   + b = b
NumS a + b = NumS (a + b)

plusAssoc :: Natural a -> Natural b -> Natural c -> Equal (a :+: (b :+: c)) ((a :+: b) :+: c)
plusAssoc  NumZ    b c = refl $ b + c
plusAssoc (NumS a) b c = EqlS $ plusAssoc a b c

-- | You need this! Copy your solution from
-- https://www.codewars.com/kata/a-plus-b-equals-b-plus-a-prove-it/haskell
plus_lemma_0 :: Natural a -> Equal a (a :+: Z)
plus_lemma_0  NumZ    = refl NumZ
plus_lemma_0 (NumS a) = EqlS $ plus_lemma_0 a

plus_lemma_1 :: Natural a -> Natural b -> Equal (S (b :+: a)) (b :+: S a)
plus_lemma_1 a  NumZ    = refl $ NumS a
plus_lemma_1 a (NumS b) = EqlS $ plus_lemma_1 a b

-- This is the proof that the kata requires.
-- | a + b = b + a
plusComm :: Natural a -> Natural b -> Equal (a :+: b) (b :+: a)
plusComm  NumZ    b = plus_lemma_0 b
plusComm (NumS a) b =
    EqlS (plusComm a b)
    >> plus_lemma_1 a b
    >> refl (b + NumS a)

-- This is the proof that the kata requires.
-- | a * b = b * a
(*) :: Natural a -> Natural b -> Natural (a :*: b)
NumZ   * _ = NumZ
NumS a * b = b + (a * b)

-- This will also be helpful
-- zeroComm :: Natural a -> Equal Z (a :*: Z)
-- zeroComm  NumZ    = EqlZ
-- zeroComm (NumS a) = zeroComm a

(<+>) :: Equal a b -> Equal c d -> Equal (a :+: c) (b :+: d)
EqlZ   <+> b = b
EqlS a <+> b = EqlS $ a <+> b

timesComm :: Natural a -> Natural b -> Equal (a :*: b) (b :*: a)
timesComm  NumZ     NumZ    = refl NumZ
timesComm  NumZ    (NumS b) = timesComm NumZ b
timesComm (NumS a)  NumZ    = timesComm a NumZ
timesComm (NumS a) (NumS b) =
    --     b + (a * S b) -> b + (a + (b * a))
    EqlS (refl b <+> timesComm a (NumS b))
    -- b + (a + (b * a)) -> b + (a + (a * b))
    >> EqlS (refl b <+> (refl a <+> timesComm b a))
    -- b + (a + (a * b)) -> (b + a) + (a * b)
    >> EqlS (plusAssoc b a (a * b))
    -- (b + a) + (a * b) -> (b + a) + (a * b)    [ Switch to right hand side ]
    >> refl (NumS $ (b + a) + (a * b))
    -- (b + a) + (a * b) -> (a + b) + (a * b)
    >> EqlS (plusComm b a <+> refl (a * b))
    -- (a + b) + (a * b) -> a + (b + a * b)
    >> EqlS (sym $ plusAssoc a b (a * b))
    --   a + (b + a * b) -> a + (b * S a)
    >> sym (EqlS $ refl a <+> timesComm b (NumS a))
    >> refl (NumS $ a + (b * NumS a))