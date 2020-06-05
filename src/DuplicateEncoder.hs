module DuplicateEncoder where

import Data.List(find)
import Data.Char(toLower)

duplicateEncode :: String -> String
duplicateEncode s = mkStr [] (toLower <$> s)
    where
        mkStr _     []        = ""
        mkStr lStk (x : rStk) = maybe "(" (const ")") (find (==x) $ lStk ++ rStk) ++ mkStr (x : lStk) rStk
