{-# LANGUAGE LambdaCase #-}
module Fixit where
import Prelude hiding (reverse, foldr)

fix :: (a -> a) -> a
fix f = let x = f x in x

reverse' :: ([a] -> [a]) -> [a] -> [a]
reverse' f = \case
    []   -> []
    x:xs -> f xs ++ [x]

foldr' :: ((a -> b -> b) -> b -> [a] -> b) -> (a -> b -> b) -> b -> [a] -> b
foldr' f g b = \case
    []   -> b
    x:xs -> g x $ f g b xs
    -- 1:[2,3,4] -> +1 . +0
    -- 2:[3,4]   -> +2 . +1 . +0
    -- 3:[4]     -> +3 . +2 . +1 . +0
    -- 4:[]      -> +4 . +3 . +2 . +1 . +0
