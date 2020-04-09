module Foldmap where

    import Data.Foldable (foldMap, Foldable)
    import Data.Monoid
    import Data.List

    myToList :: Foldable t => t a -> [a]
    myToList = foldMap (:[])
    
    myMinimum :: (Ord a, Foldable t) => t a -> Maybe a
    myMinimum ta = case sort $ foldMap (:[]) ta of
                    []    -> Nothing
                    (a:_) -> Just a

    myFoldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
    myFoldr f b ta = case foldMap (:[]) ta of
                        [] -> b
                        s  -> f' b (reverse s)
                        where
                            f' = foldl (flip f)