module ShortestList where

import Data.List hiding (tails)

shortestList :: [[a]] -> [a]
shortestList [] = []
shortestList xs = let l = dropEnd xs in shortest (repeat []) l
    where
        heads = map (take 1)
        tails = map (drop 1)
        shortest r l = maybe (shortest (zipWith (++) r (heads l)) $ tails l) (r !!) $ findIndex null l

        dropEnd  []    = []
        dropEnd ([]:_) = [[]]
        dropEnd (x:xs) = x : dropEnd xs

        -- shortest' :: [[a]] -> [[a]] -> [a]
        -- shortest' r l = case findIndex null l of
        --         Nothing -> shortest' (zipWith (++) r (heads l)) (tails l)
        --         Just i  -> r !! i



-- [[1,2,3], [4,5,6], [7,8]]
-- Result: [7,8]

-- [[1,1,1,1,1,1,1...], [4,5,6], [7,8]]
-- Result: [7,8]

-- [repeat 1, cycle [4,5,6], [7,8]]

-- [
--   [1,1,1,1,1,1,1,...],
--   [4,5,6,4,5,6,4,5,6,4,5,6,...],
--   []
-- ]