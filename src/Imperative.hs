{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
module Imperative (def, var, lit, while, (+=), (-=), (*=)) where

import Control.Monad.State (State, state, put, get, modify, runState, execState)
import Control.Lens(ix, (%~))

data Free f a = Pure a | Impure (f (Free f a))

instance Functor f => Functor (Free f) where
    fmap f (Pure   a) = Pure $ f a
    fmap f (Impure r) = Impure $ fmap f <$> r

instance Functor f => Applicative (Free f) where
    pure = Pure
    Pure   f <*> m = fmap f m
    Impure f <*> a = Impure $ fmap (<*> a) f

instance Functor f => Monad (Free f) where
    return = pure
    Pure   a >>= f = f a
    Impure m >>= f = Impure $ (>>= f) <$> m


liftF :: Functor f => f a -> Free f a
liftF f = Impure $ fmap Pure f

data Var s a = Ref s | Lit a

data Calc s a r
    = CVar a (Var s a -> r)
    | CModify (a -> a -> a) (Var s a) (Var s a) r
    | CWhile (Var s a) (a -> Bool) (FCalc s a ()) r
    deriving Functor

type FCalc s a = Free (Calc s a)

var :: a -> FCalc s a (Var s a)
var a = liftF $ CVar a id

lit :: a -> Var s a
lit = Lit

modifyC :: (a -> a -> a) -> Var s a -> Var s a -> FCalc s a ()
modifyC f v v' = liftF $ CModify f v v' ()

(+=), (-=), (*=) :: Num a => Var s a -> Var s a -> FCalc s a ()
(+=) = modifyC (+)
(*=) = modifyC (*)
(-=) = modifyC (-)

while :: Var s a -> (a -> Bool) -> FCalc s a () -> FCalc s a ()
while v p c = liftF $ CWhile v p c ()

-- return :: Num a => Var s a -> FCalc s a a
-- return v = liftF $ CReturn v id

def :: FCalc Int a (Var Int a) -> a
def free = case runState (runCalc free) [] of
    (Lit a, _) -> a
    (Ref i, l) -> l !! i

runCalc :: FCalc Int a r -> State [a] r
runCalc (Pure    a                           ) = state (a, )
runCalc (Impure (CVar    a  f               )) = get >>= \l -> put (l ++ [a]) >> runCalc (f . Ref $ length l)
runCalc (Impure (CModify _ (Lit _)  _      _)) = error "Cannot modify literal"
runCalc (Impure (CModify f (Ref i) (Lit v) r)) = get >>= \l -> put (ix i %~ flip f v $ l) >> runCalc r
runCalc (Impure (CModify f (Ref i) (Ref j) r)) = get >>= \l -> put (ix i %~ flip f (l !! j) $ l) >> runCalc r
runCalc (Impure (CWhile  v p c r            )) = get >>= \l -> put (runWhile l v p c) >> runCalc r

runWhile :: [a] -> Var Int v -> (a -> Bool) -> FCalc Int a () -> [a]
runWhile l r@(Ref i) p c =
    if p (l !! i)
        then runWhile (execState (runCalc c) l) r p c
        else l

-- factorial :: Integer -> Integer
-- factorial n = def $ do
--   result <- var 1
--   i      <- var n
--   while i (>0) $ do
--     result *= i
--     i      -= lit 1
--   return result