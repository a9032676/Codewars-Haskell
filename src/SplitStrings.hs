module SplitStrings where

solution :: String -> [String]
solution []       = []
solution [a]      = [a : ['_']]
solution (a:b:xs) = [a, b] : solution xs