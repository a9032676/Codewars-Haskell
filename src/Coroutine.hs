{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
module Coroutine where

import Control.Monad (ap, forever)

-- Preloaded contains the following:

-- r :: The final result type
-- u :: In parameter type
-- d :: Out return value type
-- a :: Done wrapped type
newtype Coroutine r u d a = Coroutine { runCoroutine :: (Command r u d a -> r) -> r } deriving (Functor)

data Command r u d a =
  Done a
    | Out d (Coroutine r u d a)
    | In (u -> Coroutine r u d a) deriving Functor

-- Useful alias
apply :: Coroutine r u d a -> (Command r u d a -> r) -> r
apply = runCoroutine

mkCo :: Command r u d a -> Coroutine r u d a
mkCo cmd = Coroutine $ \k -> k cmd

instance Applicative (Coroutine r u d) where
  pure = return
  (<*>) = ap

instance Monad (Coroutine r u d) where
  -- return x              :: a -> (Coroutine r u d a)
  -- x                     :: a
  -- Coroutine (\k -> ...) :: ((Command r u d a -> r) -> r) -> Coroutine r u d a
  -- - (\k -> ...)         :: (Command r u d a -> r) -> r
  -- - k                   :: Command r u d a -> r
  return x = Coroutine $ \k -> k $ Done x

  -- f >>= g               :: Coroutine r u d a -> (a -> Coroutine r u d b) -> Coroutine r u d b
  -- f                     :: Coroutine r u d a
  -- g                     :: a -> Coroutine r u d b
  -- Coroutine (\k -> ...) :: ((Command r u d b -> r) -> r) -> Coroutine r u d b
  -- - (\k -> ...)         :: (Command r u d b -> r) -> r
  -- - k                   :: Command r u d b -> r
  -- - apply f             :: (Command r u d a -> r) -> r
  -- - - c                 :: Coroutine r u d a

  f >>= g = Coroutine $ \k -> apply f $ \case
    Done a  -> apply (g a) k
    In uf   -> k $ In $ \u -> uf u >>= g
    Out d c -> k $ Out d (c >>= g)

-- p1 :: ... -> In
-- p2 :: In -> In ...
-- result : (... -> In) -> In -> In ...

-- p1 :: ... -> In
-- p2 :: Out -> Out ...
-- result : Out (... -> In) -> Out ...

-- p1 :: ... Out
-- p2 :: In -> In ...
-- result : (... -> Out) -> In ...

-- p1 :: (... -> Out)
-- p2 :: Out -> Out ...
-- result : Out (... -> Out) -> Out ...

(>>>) :: Coroutine r u m a -> Coroutine r m d a -> Coroutine r u d a
p1 >>> p2 = Coroutine $ \k -> apply p2 $ \case
  Done a       -> k $ Done a
  Out d coRmda -> k $ Out d $ Coroutine $ \k' -> apply (p1 >>> coRmda) k'
  -- Out d coRmda -> apply p1 $ \case
  --                   Done a       -> k $ Done a
  --                   Out _ coRuma -> k $ Out d (coRuma >>> coRmda)
  --                   In uf        -> k $ In $ \u -> uf u >>> coRmda
  In mf        -> apply p1 $ \case
                    Done a       -> k $ Done a
                    Out m coRuma -> apply (coRuma >>> mf m) k
                    In mg        -> k $ In $ \u -> mg u `pipe2` mf

-- It might be useful to define the following function

--       OutsideCo (... -> In)   InsideCo (In -> ...)         Result
pipe2 :: Coroutine r u m a -> (m -> Coroutine r m d a) -> Coroutine r u d a
p `pipe2` mf = Coroutine $ \k -> apply p $ \case
  Done a       -> k $ Done a
  Out m coRuma -> apply (coRuma >>> mf m) k
  In uf        -> k $ In $ \u -> uf u `pipe2` mf

-- Library functions

output :: a -> Coroutine r u a ()
output v = mkCo $ Out v $ return ()

input :: Coroutine r v d v
input = mkCo $ In return

produce :: [a] -> Coroutine r u a ()
produce []     = return ()
produce (x:xs) = mkCo $ Out x $ produce xs

consume :: Coroutine [t] u t a -> [t]
consume c = apply c $ \case
  Done _   -> []
  Out v c' -> v : consume c'
  In _     -> []

filterC :: (v -> Bool) -> Coroutine r v v ()
filterC p = mkCo $ In $ \v ->
  if p v
    then mkCo $ Out v $ filterC p
    else filterC p

mapC :: (v -> v) -> Coroutine r v v ()
mapC f = mkCo $ In $ \v -> mkCo $ Out (f v) (mapC f)

limit :: Int -> Coroutine r v v ()
limit 0 = return ()
limit n = if n < 0 then limit 0 else
  mkCo $ In $ \v ->
    mkCo $ Out v $ limit (n-1)

suppress :: Int -> Coroutine r v v ()
suppress 0 = mkCo $ In $ \v -> mkCo $ Out v (suppress 0)
suppress n = mkCo $ In $ \_ -> suppress (n-1)

add :: Coroutine r Int Int ()
add = mkCo $ In $ \v ->
  mkCo $ In $ \v' ->
    mkCo $ Out (v + v') add

duplicate :: Coroutine r v v ()
duplicate = mkCo $ In $ \v ->
  mkCo $ Out v $
  mkCo $ Out v duplicate

-- Programs
-- 1. A program which outputs the first 5 even numbers of a stream.
-- 2. A program which produces a stream of the triangle numbers 
-- 3. A program which multiplies a stream by 2
-- 4. A program which sums adjacent pairs of integers

p1, p2, p3, p4 :: Coroutine r Int Int ()

p1 = filterC even >>> limit 5
p2 = produce [1..] >>> mapC (\v -> v * (v + 1) `div` 2)
p3 = mapC (*2)
p4 = Coroutine $ \k -> k $ In $ \v ->
    Coroutine $ \k' -> k' $ In $ \v' ->
      Coroutine $ \_ -> k' $ Out (v + v') p4