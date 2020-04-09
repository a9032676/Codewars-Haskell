{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module YonedaLemma where
import YonedaLemmaPreloaded
import Data.Functor.Contravariant
import Data.Void

-- Hom(a, b) ≡ all arrows/morphisms from object `a` to object `b`
-- in given category.
-- Hom(a, -) covariant functor:
type Hom a = (->) a

-- natural transformation from functor f to functor g:
type Nat f g = forall x. f x -> g x

-- in order to witness isomorphism
-- we should provide `to` and `from` such, that
-- to . from ≡ id[f a]
-- from . to ≡ id[Nat (Hom a) f]

-- ((Hom a) a -> (f) a) -> f a
-- Using 'id' to natural transformation from (Hom a) -> f
to :: Functor f => Nat (Hom a) f -> f a
to nat = nat id

-- f a -> ((Hom a) x -> (f) x)
-- Using 'fmap' to mapping functor from f a -> f x by (Hom a x)
from :: Functor f => f a -> Nat (Hom a) f
from fa hom = fmap hom fa


-- Hom(-, a) contravariant functor:
type CoHom a = Op a
{- NOTE:
Op a b = Op { getOp :: b -> a }

class Contravariant f where
  contramap :: (b -> a) -> f a -> f b
-}

-- ((CoHom a) a -> (f) a) -> f a
to' :: Contravariant f => Nat (CoHom a) f -> f a
to' nat = nat $ Op id

from' :: Contravariant f => f a -> Nat (CoHom a) f
from' fa cohom = contramap (getOp cohom) fa

-- now we will try to count the natural transformations

{-
newtype Count x = Count { getCount :: Int } deriving (Show, Eq)
coerce :: Count a -> Count b
class Countable where count :: Count c
class Factor where factor :: Countable c => Count (f c)
instance (Factor f, Countable c) => Countable (f c) where count = factor
-}

-- | NOTE: from here onwards you should imagine `forall x` inside `Count (...)`,
-- | i. e., not `Count ((Numbers -> x) -> Maybe x)`, but `Count (forall x. (Numbers -> x) -> Maybe x)`
-- | we are unable to write it because GHC doesn't yet support impredicative polymorphism (see issue: https://www.codewars.com/kata/yoneda-lemma/discuss/haskell#5b0f4afd3aa7cf7eb100000e)

-- x ^ c
count1' :: forall c x. (Countable c, Countable x) => Count (c -> x)
count1' = liftC2 (^) (count @x) (count @c)

count1'' :: forall f x. (Functor f, Factor f, Countable x) => Count (f x)
count1'' = factor

-- (f x) ^ (x ^ c)
count1 :: forall f c x. (Functor f, Factor f, Countable c) => Count ((c -> x) -> f x)
count1 = coerce $ count @(f c)

count2 :: forall f c x. (Contravariant f, Factor f, Countable c) => Count ((x -> c) -> f x)
count2 = coerce $ count @(f c)
-- | TIP: you could use types `f`, `c` in RHS of count1 and count2
-- | (because of ScopedTypeVariables pragma and explicit forall)

-- and now i encourage you to count something on fingers ;)
data Numbers = One | Two | Three deriving (Show, Eq)

instance Countable Numbers where
  count = Count 3

--               x
-- Hom (Numbers, -) Maybe ---> Maybe Numbers
-- 3 + 1 = 4
challenge1 :: Count ((Numbers -> x) -> Maybe x)
challenge1 = Count 4

--                     x  x
-- Hom (Maybe Numbers, -) - ---> Maybe Numbers
-- 3 + 1 = 4
challenge2 :: Count ((Maybe Numbers -> x) -> x)
challenge2 = Count 4

--               x             x        (Bool -> Numbers)
-- Hom (Numbers, -) ((->) Bool -) ---> ((->) Bool) Numbers
-- Numbers ^ Bool = Count 9
--       3 ^ 2    = 9
challenge3 :: Count ((Numbers -> x) -> (Bool -> x))
challenge3 = Count 9

{- Void is a data type without constructors, its declaration:
data Void
Predicate x = Predicate { getPredicate :: x -> Bool }
-- as you might have noticed, Predicate is Contravariant
-}

-- Count ((x -> Void) -> (x -> Bool)) :
--      x              x              (Void -> Bool)
-- Hom (-, Void) ((->) - Bool) ---> ((->) - Bool) Void
-- Bool ^ Void = Count 1
--    2 ^ 0    = 1
challenge4 :: Count ((x -> Void) -> Predicate x)
challenge4 = Count 1

-- (forall y. (Bool -> y) -> (Numbers -> y))) :
--            y                y        (Numbers -> Bool)
-- Hom (Bool, -) ((->) Numbers -) ---> ((->) Numbers Bool)
-- Bool ^ Numbers = Count 8
--    2 ^ 3       = 8

--      x             y                y        (Numbers -> Bool)         x                 (Numbers -> Bool) -> Numbers
-- Hom (-, Hom (Bool, -) ((->) Numbers -) ---> ((->) Numbers Bool)) ((->) - Numbers) ---> ((->) (Numbers -> Bool) Numbers)
-- Numbers ^ (Bool ^ Numbers) = Count 6561
--       3 ^    (2 ^ 3)       = 6561
-- challenge5 :: Count (forall x. (x -> (forall y. (Bool -> y) -> (Numbers -> y))) -> (x -> Numbers))
challenge5 :: Count ((x -> ((Bool -> y) -> (Numbers -> y))) -> (x -> Numbers))
challenge5 = Count 6561