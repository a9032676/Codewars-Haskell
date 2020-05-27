module IPv4 where

import Data.Bits (shiftR, (.&.))
import Data.Word (Word32)
import Data.List (intercalate)

type IPString = String

word32ToIP :: Word32 -> IPString
word32ToIP ip = intercalate "." $ map (\i -> show $ (ip `shiftR` (i * 8)) .&. 0xFF) [3,2,1,0]