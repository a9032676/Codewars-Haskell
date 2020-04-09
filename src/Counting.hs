{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Counting where
import CountingPreloaded
import Data.Proxy
import Data.Void
import Data.Functor.Const
import Data.Functor.Identity
import Data.Functor.Sum
import Data.Functor.Product
import Data.Functor.Compose
import Data.List
import           Control.Monad

{-
data Nat = Z | S Nat deriving (Show, Eq, Ord)
instance Num Nat-- so (2 :: Nat) == S (S Z)
instance Enum Nat -- so ([0..3] :: [Nat]) == [Z, S Z, S (S Z)]
instance Real Nat
instance Integral Nat -- so (2 ^ 3 :: Nat) == S (S (S (S (S (S (S (S Z)))))))
-}

newtype Count x = Count { getCount :: Nat } deriving (Show, Eq, Ord)

-- | helper functions
mapC :: (Nat -> Nat) -> Count a -> Count b
mapC f (Count a) = Count $ f a

liftC2 :: (Nat -> Nat -> Nat) -> Count a -> Count b -> Count c
liftC2 f (Count a) (Count b) = Count $ f a b

coerceC :: Count a -> Count b
coerceC (Count a) = Count a

-- | Countable
class Countable c where
  count :: Count c
  -- if you are using `Proxy` implement `count` from `count'` and vice versa
  -- count' :: Proxy c -> Count c
  -- count' = error "from count"

instance Countable Void where count = Count 0
instance Countable () where count = Count 1
instance Countable Bool where count = Count 2
instance Countable Nat where count = mapC S $ count @Nat

-- | Factor
class Factor f where
  factor :: Count c -> Count (f c)
  -- factor' :: Proxy f -> Count c -> Count (f c) -- optional

instance (Factor f, Countable c) => Countable (f c) where
  count = factor $ count @c

instance Factor Maybe where factor = Count . S . getCount
instance Factor Identity where factor = coerceC
instance Factor Proxy where factor _ = Count 1
instance Factor Count where factor = liftC2 (+) $ count @Nat
instance Factor [] where
  factor (Count 0) = Count 1
  factor _ = mapC (*2) $ count @Nat
instance Countable c => Factor (Const c) where factor _ = coerceC $ count @c
instance Countable c => Factor (Either c) where factor = liftC2 (+) $ count @c
instance Countable c => Factor ((,) c) where factor = liftC2 (*) (count @c)
instance Countable c => Factor ((->) c) where factor c = liftC2 (^) c $ count @c
instance (Factor f, Factor g) => Factor (Sum f g) where
  factor c = liftC2 (+) (factor @f c) (factor @g c)
instance (Factor f, Factor g) => Factor (Product f g) where 
  factor c = liftC2 (*) (factor @f c) (factor @g c)
instance (Factor f, Factor g) => Factor (Compose f g) where
  factor c = coerceC $ factor @f (factor @g c)

-- | Listable
class Countable a => Listable a where
  list :: [a]
  -- list' :: Proxy a -> [a] -- optional
-- Data.List.genericLength (list :: [a]) `shouldBe` getCount (count :: Count a)

instance Listable Void where list = []
instance Listable () where list = [()]
instance Listable Bool where list = [True, False]
instance Listable Nat where list = [0..]

instance Listable c => Listable (Maybe c) where list = Nothing : (Just <$> list @c)
instance Listable c => Listable [c] where list = iterate (\c -> c ++ list @c) []
instance (Listable a, Listable b) => Listable (Either a b) where list = (Left <$> list @a) ++ (Right <$> list @b)
-- instance (Listable a, Listable b) => Listable (a, b) where list = concat [[ (a, b) | a <- list @a] | b <- list @b]
instance (Listable a, Listable b) => Listable (a, b) where list = liftM2 (,) (list @a) (list @b)
instance (Eq a, Listable a, Listable b) => Listable (a -> b) where
  list = case count @a of
      Count 0 -> [const . head $ list @b]
      Count 1 -> foldl (\abs b -> abs ++ [const b]) [] (list @b)
      Count n -> toFunc (list @a) <$> pow n
    where
      pow n = pow' n $ (: []) <$> list @b
      pow' 1 xs = xs
      pow' n xs = pow' (n-1) $ foldl (\powS b -> powS ++ ((\s -> s ++ [b]) <$> xs)) [] $ list @b

      toFunc :: [a] -> [b] -> a -> b
      toFunc as bs a = snd . head . filter (\(a', _) -> a == a') $ zip as bs


-- 2 ^ 3 = 8
-- [
-- ((True,True),True),      [1]
-- ((False,True),True),     [2]
-- --((True,True),True),
-- ((True,False),True),     [3]
-- --((True,True),True),
-- ((True,True),False),     [4]
-- ((True,False),False),    [5]
-- ((False,False),False),   [6]
-- ((False,True),False),    [7]
-- --((False,False),False),
-- ((False,False),True),    [8]
-- --((False,False),False)
-- ]

-- 3 ^ 2 = 9
-- [
-- (Nothing,Nothing),         [1]
-- (Just True,Nothing),       [2]
-- (Just False,Nothing),      [3]
-- --(Nothing,Nothing),
-- (Nothing,Just True),       [4]
-- (Nothing,Just False),      [5]
-- --(Nothing,Just True),
-- (Just True,Just True),     [6]
-- (Just False,Just True),    [7]
-- --(Just True,Nothing),
-- --(Just True,Just True),
-- (Just True,Just False),    [8]
-- --(Nothing,Just False),
-- --(Just True,Just False),
-- (Just False,Just False),   [9]
-- --(Just False,Nothing),
-- --(Just False,Just True),
-- --(Just False,Just False)
-- ]

-- 3 ^ 2 = 9
-- Listable (Bool -> Maybe Bool) = [
--  [True -> Nothing   , False -> Nothing   ],
--  [True -> Nothing   , False -> Just True ],
--  [True -> Nothing   , False -> Just False],
--  [True -> Just True , False -> Nothing   ],
--  [True -> Just True , False -> Just True ],
--  [True -> Just True , False -> Just False],
--  [True -> Just False, False -> Nothing   ],
--  [True -> Just False, False -> Just True ],
--  [True -> Just False, False -> Just False]
--]

-- Listable (Maybe Bool -> Bool) = [
--  [Nothing -> True , Just True -> True , Just False -> True ],
--  [Nothing -> True , Just True -> True , Just False -> False],
--  [Nothing -> True , Just True -> False, Just False -> True ],
--  [Nothing -> True , Just True -> False, Just False -> False],
--  [Nothing -> False, Just True -> True , Just False -> True ],
--  [Nothing -> False, Just True -> True , Just False -> False],
--  [Nothing -> False, Just True -> False, Just False -> True ],
--  [Nothing -> False, Just True -> False, Just False -> False],
--]
