{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module PolyvariadicFunctions where

    class (Integral a) => IntVariadic a r | r -> a where
        accumulate :: (Integral a) => a -> r

    instance IntVariadic Int Int where
        accumulate = fromIntegral

    instance (Integral a, IntVariadic a r) => IntVariadic a (a -> r) where
        accumulate acc = accumulate . (acc+) . fromIntegral

    -- `polyAdd` sums its arguments, all `Int`s.
    polyAdd :: (Integral a, IntVariadic a r) => r
    polyAdd = accumulate 0

    class ListVariadic a r | r -> a where
        join :: [a] -> r

    instance ListVariadic a [a] where
        join = id

    instance (ListVariadic a r) => ListVariadic a (a -> r) where
        join acc next = join $ acc ++ [next]

    -- `polyList` turns its arguments into a list, polymorphically.
    polyList :: (ListVariadic a r) => r
    polyList = join []

    class StrVariadic r where
        append :: String -> r

    instance StrVariadic String where
        append = id

    instance StrVariadic r => StrVariadic (String -> r) where
        append ""  next = append next
        append acc next = append $ acc ++ " " ++ next

    -- `polyWords` turns its arguments into a spaced string.
    polyWords :: (StrVariadic r) => r
    polyWords = append ""
