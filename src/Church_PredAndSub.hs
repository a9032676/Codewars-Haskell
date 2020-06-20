{-# LANGUAGE RankNTypes #-}
module Church_PredAndSub where

import Prelude hiding (succ, pred, fst, snd)

one :: Number
one = succ zero

pred :: Number -> Number
pred (Nr n) = Nr $ \f x -> n (\g h -> h (g f)) (const x) id

subt :: Number -> Number -> Number
subt m (Nr n) = n pred m

-- Preloaded

newtype Pair a b = Pr (forall c . (a -> b -> c) -> c)

instance (Show a, Show b) => Show (Pair a b) where
  show (Pr p) = p (\ a b -> "(" ++ show a ++ "," ++ show b ++ ")")

pair :: a -> b -> Pair a b
pair f s = Pr (\ b -> b f s)

fst :: Pair a b -> a
fst (Pr p) = p const

snd :: Pair a b -> b
snd (Pr p) = p (\ _ b -> b)

newtype Number = Nr (forall a. (a -> a) -> a -> a)

instance Show Number where
  show (Nr a) = a ("1+" ++) "0"
  
instance Eq Number where
  a == b = eval a == eval b
  
fold :: Number -> (a -> a) -> a -> a
fold (Nr n) = n

eval :: Number -> Integer
eval (Nr a) = a (+1) 0

zero :: Number
zero = Nr (\ _ z -> z)

succ :: Number -> Number
succ (Nr a) = Nr (\ s z -> s (a s z))

add :: Number -> Number -> Number
add (Nr a) = a succ

mult :: Number -> Number -> Number
mult (Nr a) b =  a (add b) zero