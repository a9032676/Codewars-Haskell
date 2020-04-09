{-# LANGUAGE FlexibleInstances #-}
module YonedaLemmaPreloaded where
import Data.Proxy

newtype Count x = Count { getCount :: Int } deriving (Show, Eq)

mapC :: (Int -> Int) -> Count x -> Count y
mapC f (Count x) = Count $ f x

liftC2 :: (Int -> Int -> Int) -> Count x -> Count y -> Count z
liftC2 op (Count x) (Count y) = Count $ x `op` y

coerce :: Count a -> Count b
coerce (Count n) = Count n

class Countable c where
  count' :: Proxy c -> Count c
  count' Proxy = count
  count :: Count c
  count = count' Proxy

class Factor f where
  factor' :: Countable c => Proxy c -> Count (f c)
  factor' Proxy = factor
  factor :: Countable c => Count (f c)
  factor = factor' Proxy

instance (Factor f, Countable c) => Countable (f c) where
  count = factor