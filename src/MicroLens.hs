{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}

module MicroLens where

import Prelude hiding (sum)
import Data.Monoid
import Control.Applicative
import qualified Data.Traversable as T
import Data.Tuple (swap)

---------------------------------------------------------
-- Some basic libraries

class Profunctor p where
  dimap :: (a' -> a) -> (b -> b') -> (p a b -> p a' b')
  dimap f g = lmap f . rmap g
  lmap ::  (a' -> a) -> (p a b -> p a' b)
  lmap f = dimap f id
  rmap ::  (b -> b') -> (p a b -> p a b')
  rmap f = dimap id f
  
class Profunctor p => Choice p where
  left'  :: p a b -> p (Either a c) (Either b c)
  right' :: p a b -> p (Either c a) (Either c b)
  
instance Profunctor (->) where
  dimap f g h = g . h . f
  
instance Choice (->) where
  left'  f = either (Left . f) Right
  right' f = either Left (Right . f)
  
class Contravariant f where
  contramap :: (a' -> a) -> (f a -> f a')

-- Control.Applicative.Const replicated here for your
-- convenience
newtype K b a = K { getK :: b } deriving Functor

instance Monoid b => Applicative (K b) where
  pure _ = K mempty
  K e <*> K f = K (e <> f)

instance Contravariant (K b) where
  contramap f (K b) = K b

newtype Id a = Id { getId :: a } deriving Functor

instance Applicative Id where
  pure = Id
  Id f <*> Id x = Id (f x)

---------------------------------------------------------
-- The lens types you'll implement
  
-- | Optic is the general pattern for all other lens types.
type Optic p f s t a b =
  p a (f b) -> p s (f t)
  
type Iso s t a b =
  forall p f . (Profunctor p, Functor f) =>
  Optic p f s t a b
  
type Lens s t a b =
  forall f . Functor f => 
  Optic (->) f s t a b
  
type Traversal s t a b =
  forall f . Applicative f =>
  Optic (->) f s t a b
  
type Fold s a = 
  forall f . (Contravariant f, Applicative f) =>
  Optic (->) f s s a a

type Prism s t a b =
  forall p f . (Choice p, Applicative f) =>
  Optic p f s t a b
    
---------------------------------------------------------
---------------------------------------------------------
-- Todo

-- | A lens focusing on the first element in a pair

-- type Optic p f s t a b = p a (f b) -> p s (f t)
-- type Lens s t a b = forall f . Functor f => Optic (->) f s t a b
-- Lens (a, x) (b, x) a b
-- := Optic (->) f s t a b
-- := Optic (->) f (a, x) (b, x) a b
-- := (a -> (f b)) -> ((a, x) -> (f (b, x)))
_1 :: Lens (a, x) (b, x) a b
_1 f (a, x) = (, x) <$> f a

-- | A lens focusing on the second element in a pair
_2 :: Lens (x, a) (x, b) a b
_2 f (x, a) = (x, ) <$> f a

-- | A function which takes a lens and looks through it.
-- The type given is specialized to provide a hint as to
-- how to write 'view'. The more intuitive type for its use
-- is
--
-- @
-- view :: Lens s t a b -> (s -> a)
-- @

-- type Lens s t a b = Functor f => (a -> f b) -> s -> f t
-- type Optic p f s t a b = p a (f b) -> p s (f t)
-- := Optic (->) (K a) s t a b
-- := a -> ((K a) b) -> s -> ((K a) t)
-- := (a -> (K a b)) -> (s -> (K a t))
view :: Optic (->) (K a) s t a b -> (s -> a)
view l = getK . l K

-- | A function which takes a lens and a transformation function
-- and applies that transformer at the focal point of the lens.
-- The type given is specialized to provide a hint as to how to
-- write 'over'. The more intuitive type for its use is
--
-- @
-- over :: Lens s t a b -> (a -> b) -> (s -> t)
-- @
over :: Optic (->) Id s t a b -> (a -> b) -> (s -> t)
over l f = getId . l (Id . f)

-- | A function from a lens and a value which sets the value
-- at the focal point of the lens. The type given has been
-- specialized to provide a hint as to how to write 'set'. The
-- more intuitive type for its use is
--
-- @
-- set :: Lens s t a b -> b -> (s -> t)
-- @
set :: Optic (->) Id s t a b -> b -> (s -> t)
set l b = getId . l (const . Id $ b)

-- | A traversal which focuses on each element in any 
-- Traversable container.
elements :: T.Traversable f => Traversal (f a) (f b) a b
elements = traverse

-- | A function which takes a Traversal and pulls out each 
-- element it focuses on in order. The type has been 
-- specialized, as the others, but a more normal type might be
--
-- @
-- toListOf :: Traversal s s a a -> (s -> [a])
-- @

-- Optic (->) (K (Endo [a])) s s a a
-- p a (f b) -> p s (f t)
-- := (a -> (K (Endo [a]) b)) -> (s -> (K (Endo [a]) s))
--
toListOf :: Optic (->) (K (Endo [a])) s s a a -> (s -> [a])
toListOf l = flip appEndo [] . getK . l (\a -> K . Endo $ (a:))

-- | A function which takes any kind of Optic which might
-- be focused on zero subparts and returns Just the first
-- subpart or else Nothing.
--
-- @
-- preview :: Traversal s s a a -> (s -> Maybe a)
-- @

-- Optic (->) (K (First a)) s s a a
-- p a (f b) -> p s (f t)
-- := (a -> (K (First a) a) -> (s -> (K (First a) s))
preview :: Optic (->) (K (First a)) s s a a -> (s -> Maybe a)
preview l = getFirst . getK . l (K . First . Just)

-- | A helper function which witnesses the fact that any
-- container which is both a Functor and a Contravariant
-- must actually be empty.
coerce :: (Contravariant f, Functor f) => f a -> f b
coerce = contramap (const ()) . fmap (const ())

-- | A Fold which views the result of a function application

-- := (a -> (f a)) -> (s -> (f s))
to :: (a -> b) -> Fold a b
to f g = coerce <$> g . f

-- | A prism which focuses on the left branch of an Either

-- (p a (f b)) -> (p (Either a x) (f (Either b x)))
-- := (a -> (f b)) -> (Either a x -> (f (Either b x)))

-- Either (f b) x -> f (Either b x)
-- := t (f b) x -> f (t b x)
_Left :: Prism (Either a x) (Either b x) a b
_Left = rmap (either (fmap Left) (pure . Right)) . left'
  
-- | A prism which focuses on the right branch of an Either
_Right :: Prism (Either x a) (Either x b) a b
-- p (Any a) (Either Any (f b)) -> p (Either x a) (f (Either x b))
_Right = rmap (either (pure . Left) (fmap Right)) . right'

-- | An iso which witnesses that tuples can be flipped without
-- losing any information

-- ((b, a) -> f (b, a)) -> ((a, b) -> f (a, b))
-- := (p (b, a) (f (b, a))) -> (p (a, b) (f (a, b)))
_flip :: Iso (a, b) (a, b) (b, a) (b, a)
_flip = dimap swap (fmap swap)
