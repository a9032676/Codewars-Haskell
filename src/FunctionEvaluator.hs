module FunctionEvaluator where

evaluateFunction :: Ord a => (a -> Either b ([a], [b] -> b)) -> a -> b
evaluateFunction f a =
    case f a of
        Left b        -> b
        Right (as, g) -> g $ evaluateFunction f <$> as



foo  i | i <= 2    = Left 1
       | odd i     = Right ([6*i`div`7, 2*i`div`3], sum)
       | otherwise = Right ([i-1, i-3], sum)

fibonacci i | i < 2     = Left i
            | otherwise = Right ([i-1, i-2], sum)

{-


eval := evaluateFunction

eval := g $ (g $ (g $ ... (g_n $ eval f_n <$> as_n) f <$> as) f <$> as) f <$> as

fib 5
-> [fib 4, fib 3]
-> [[fib 3, fib 2], [fib 2, fib 1]]
-> [[ [fib 2, fib 1], [fib 1, fib 0] ], [ [fib 1, fib 0], 1 ]]
-> [[ [ [fib 1, fib 0], 1], [1, 0] ], [ [1, 0], 1 ]]
-> [[ [ [1, 0], 1], [1, 0] ], [ [1, 0], 1 ]]

fib 5
fib 5 (fib )

-}