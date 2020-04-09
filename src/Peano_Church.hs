{-# LANGUAGE 
  FlexibleInstances, 
  UndecidableInstances, 
  InstanceSigs,
  ScopedTypeVariables,
  RankNTypes #-}

module Peano_Church where

import Data.List

type ISO a b = (a -> b, b -> a)
-- See https://www.codewars.com/kata/isomorphism

symm :: ISO a b -> ISO b a
symm (ab, ba) = (ba, ab)

substL :: ISO a b -> (a -> b)
substL = fst

substR :: ISO a b -> (b -> a)
substR = snd

-- ((a -> a -> a) -> (b -> b -> b), (b -> b -> b) -> (a -> a -> a))
liftISO2 :: ISO a b -> ISO (a -> a -> a) (b -> b -> b)
liftISO2 (ab, ba) = (\fa b b' -> ab $ fa (ba b) (ba b'), \fb a a' -> ba $ fb (ab a) (ab a'))

-- A Natural Number is either Zero,
-- or a Successor (1 +) of Natural Number.
-- We have (+)/(*) on Natural Number, or (-) it.
-- Since Natrual Number do not have negative, forall x, 0 - x = 0.
-- We also have pow on Natrual Number
-- Since Haskell is lazy, we also have infinity

class Nat n where
  zero :: n
  successor :: n -> n
  nat :: a -> (n -> a) -> n -> a -- Pattern Matching
  iter :: a -> (a -> a) -> n -> a -- Induction
  plus, minus, mult, pow :: n -> n -> n
  inf :: n
  inf = successor inf
  divide :: n -> n -> n
  l `divide` r | l == 0 && r == 0 = undefined
  l `divide` r | l < r = 0
  l `divide` r | otherwise = successor $ (l `minus` r) `divide` r
  -- notice (l `divide` 0) when l is not 0 will return inf
  isoP :: ISO n Peano -- See below for the definition of Peano
  isoP = (iter zero successor, iter zero successor)
  toP :: n -> Peano
  toP = substL isoP

instance {-# OVERLAPPABLE #-} Nat n => Show n where
  show = show . toP

instance {-# OVERLAPPABLE #-} Nat n => Eq n where
  l == r = toP l == toP r

instance {-# OVERLAPPABLE #-} Nat n => Ord n where
  l `compare` r = toP l `compare` toP r

instance {-# OVERLAPPABLE #-} Nat n => Num n where
  abs = id
  signum = nat zero (const 1)
  fromInteger 0 = zero
  fromInteger n | n > 0 = successor $ fromInteger (n - 1)
  (+) = plus
  (*) = mult
  (-) = minus

-- We can encode Natrual Number directly as Algebraic Data Type(ADT).
data Peano = O | S Peano deriving (Show, Eq, Ord)

-- Remember, 0 - x = 0 for all x.
instance Nat Peano where
    zero = O
    successor = S

    nat z _   O    = z
    nat _ sn (S n) = sn n
    
    iter a _     O    = a
    iter a succ (S n) = iter (succ a) succ n
    
    plus  O    n = n
    plus (S m) n = S (plus m n)
    
    minus  m     O    = m
    minus  O     _    = O
    minus (S m) (S n) = minus m n
    
    mult  O    _ = O
    mult (S m) n = plus n (mult m n)
    
    pow _  O    = S O
    pow O  _    = O
    pow m (S n) = mult m (pow m n)

-- Peano is very similar to a basic data type in Haskell - List!
-- O is like [], and S is like :: (except it lack the head part)
-- When we want to store no information, we can use (), a empty tuple
-- This is different from storing nothing (called Void in Haskell),
-- as we can create a value of () by using (), 
-- but we cannot create a value of Void.

-- Notice how you can implement everything once you have isoP,
-- By converting to Peano and using Nat Peano?
-- Dont do that. You wont learn anything.
-- Try to use operation specific to list.
instance Nat [()] where
    zero = []
    successor l = ():l

    nat z _   []     = z
    nat _ sn (():xs) = sn xs
    
    iter a _     []     = a
    iter a succ (():xs) = iter (succ a) succ xs

    plus  []    n = n
    plus (():m) n = ():plus m n

    minus  m      []    = m
    minus  []     _     = []
    minus (():m) (():n) = minus m n

    mult  []    _ = []
    mult (():m) n = plus n (mult m n)

    pow _  []    = [()]
    pow [] _     = []
    pow m (():n) = mult m (pow m n)

-- Instead of defining Nat from zero, sucessor (and get Peano),
-- We can define it from Pattern Matching
newtype Scott = Scott { runScott :: forall a. a -> (Scott -> a) -> a }
instance Nat Scott where

    zero = Scott const
    successor s = Scott $ \_ sa -> sa s

    nat z sn (Scott s) = s z sn

    -- a -> (a -> a) -> Scott                                       -> a
    -- a -> (a -> a) -> (a -> (Scott -> a) -> a)                    -> a
    -- a -> (a -> a) -> (a -> ((a -> (Scott -> a) -> a) -> a) -> a) -> a
    iter a succ (Scott s) = s a $ \scott -> iter (succ a) succ scott

    -- Other operation on Scott numeral is sort of boring,
    -- So we implement it using operation on Peano.
    -- You shouldnt do this - I had handled all the boring case for you.
    plus  = substR (liftISO2 isoP) plus
    minus = substR (liftISO2 isoP) minus
    mult  = substR (liftISO2 isoP) mult
    pow   = substR (liftISO2 isoP) pow

-- Or from induction!
newtype Church = Church { runChurch :: forall a. (a -> a) -> a -> a }
instance Nat Church where
    -- Try to implement the calculation (except minus) in the primitive way.
    -- Implement them by constructing Church explicitly.
    -- So plus should not use successor,
    -- mult should not use plus,
    -- exp should not use mult.
    zero = Church $ \_ x -> x
    successor c = Church $ \f -> f . runChurch c f

    nat z sn (Church c) = let pred f x = c (\g h -> h (g f)) (const x) id in c (\_ -> sn $ Church pred) z

    -- zero := \f x. x       -> (\f x. x) succ a                         -> a
    -- one  := \f x. f x     -> (\f x. f x) succ a                       -> succ a
    -- two  := \f x. f (f x) -> (\f x. f (f x)) succ (succ a)            -> succ (succ a)
    -- n..  := \f x. n (f x) -> (\f x. f (f (f x))) succ (succ (succ a)) -> succ (succ (succ a))
    iter a succ (Church c) = c succ a

    Church m `plus`  Church n = Church $ \f x -> m f (n f x)
    Church m `minus` Church n = Church $ \f x -> undefined
    Church m `mult`  Church n = Church $ \f x -> m (n f) x
    Church m `pow`   Church n = Church $ n m