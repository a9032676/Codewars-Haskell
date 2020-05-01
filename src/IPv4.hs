module IPv4 where

import Data.Bits (shiftR, (.&.))
import Data.Word (Word32)
import Data.List (intercalate)

type IPString = String

-- int32ToIP :: Word32 -> IPString
-- int32ToIP int32 = let (x:xs) = map (show :: Integer -> String) (patchBits . segmentIP $ fromIntegral int32) in foldl (\x y -> x ++ "." ++ y) x xs

-- patchBits :: (Integral a, Bits a) => [a] -> [a]
-- patchBits segIP = case length segIP of
--     0 -> [0, 0, 0, 0]
--     1 -> [0, 0, 0] ++ segIP
--     2 -> [0, 0] ++ segIP
--     3 -> 0 : segIP
--     4 -> segIP

-- segmentIP :: (Integral a, Bits a) => a -> [a]
-- segmentIP i
--   | i == 0    = []
--   | i <  0    = let neg = (2^32) + i in segmentIP (neg `shiftR` 8) ++ [neg .&. 0xFF]
--   | i >= 2^32 = [255, 255, 255, 255]
--   | otherwise = segmentIP (i `shiftR` 8) ++ [i .&. 0xFF]

word32ToIP :: Word32 -> IPString
word32ToIP ip = intercalate "." $ map (\i -> show $ (ip `shiftR` (i * 8)) .&. 0xFF) [3,2,1,0]