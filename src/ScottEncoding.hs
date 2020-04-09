{-# LANGUAGE Rank2Types #-}
module ScottEncoding where

import Prelude hiding (null, length, map, foldl, foldr, take, fst, snd, curry, uncurry, concat, zip, (++))

newtype SMaybe a = SMaybe { runMaybe :: forall b. b -> (a -> b) -> b }
newtype SList a = SList { runList :: forall b. b -> (a -> SList a -> b) -> b }
newtype SEither a b = SEither { runEither :: forall c. (a -> c) -> (b -> c) -> c }
newtype SPair a b = SPair { runPair :: forall c. (a -> b -> c) -> c }

toPair :: SPair a b -> (a, b)
toPair sp = runPair sp (\a b -> (a, b))
fromPair :: (a, b) -> SPair a b
fromPair (a, b) = SPair $ \f -> f a b
fst :: SPair a b -> a
fst sp = runPair sp const
snd :: SPair a b -> b
snd sp = runPair sp $ flip const
swap :: SPair a b -> SPair b a
swap sp = SPair $ \g -> runPair sp $ flip g
curry :: (SPair a b -> c) -> (a -> b -> c)
curry f a b = f $ SPair $ \f' -> f' a b
uncurry :: (a -> b -> c) -> (SPair a b -> c)
uncurry f g = runPair g f

toMaybe :: SMaybe a -> Maybe a
toMaybe sm = runMaybe sm Nothing Just
fromMaybe :: Maybe a -> SMaybe a
fromMaybe m = SMaybe $ \b ab -> maybe b ab m
isJust :: SMaybe a -> Bool
isJust sm = runMaybe sm False (const True)
isNothing :: SMaybe a -> Bool
isNothing = not . isJust
catMaybes :: SList (SMaybe a) -> SList a
catMaybes = foldr (\sm sl -> runMaybe sm sl (`cons` sl)) empty

toEither :: SEither a b -> Either a b
toEither se = runEither se Left Right
fromEither :: Either a b -> SEither a b
fromEither e = SEither $ \ac bc -> either ac bc e
isLeft :: SEither a b -> Bool
isLeft se = runEither se (const True) (const False)
isRight :: SEither a b -> Bool
isRight = not . isLeft

empty :: SList a
empty = SList const
partition :: SList (SEither a b) -> SPair (SList a) (SList b)
partition = foldr (\se sp -> runEither se (lp sp) (rp sp)) (SPair $ \f -> f empty empty)
    where
        lp sp a = SPair $ \f -> f (cons a $ fst sp) (snd sp)
        rp sp b = SPair $ \f -> f (fst sp) (cons b $ snd sp)

toList :: SList a -> [a]
toList sl = runList sl [] (\a sl' -> a : toList sl')
fromList :: [a] -> SList a
fromList []     = empty
fromList (s:xs) = SList $ \_ f -> f s (fromList xs)
cons :: a -> SList a -> SList a
cons a sl = SList $ \_ f -> f a sl
concat :: SList a -> SList a -> SList a
concat sl1 sl2 = runList sl1 sl2 (\a sl' -> cons a (concat sl' sl2))
null :: SList a -> Bool
null sl = runList sl True (\_ _ -> False)
length :: SList a -> Int
length = foldr (\_ i -> i + 1) 0
map :: (a -> b) -> SList a -> SList b
map f = foldr (\a slb -> cons (f a) slb) empty
zip :: SList a -> SList b -> SList (SPair a b)
zip sla slb = runList sla empty (\a sla' -> runList slb empty (\b slb' -> cons (SPair $ \f -> f a b) (zip sla' slb')))
foldl :: (b -> a -> b) -> b -> SList a -> b
foldl f b sla = runList sla b (\a sla' -> foldl f (f b a) sla')
foldr :: (a -> b -> b) -> b -> SList a -> b
foldr f b sla = runList sla b (\a sla' -> f a $ foldr f b sla')
take :: Int -> SList a -> SList a
take 0 _ = empty
take i sl = runList sl empty (\a sl' -> cons a (take (i-1) sl'))
