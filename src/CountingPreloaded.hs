module CountingPreloaded where
import Control.Arrow ((***))

data Nat = Z | S Nat deriving (Show, Eq, Ord)

nat :: a -> (Nat -> a) -> Nat -> a
nat a _ Z = a
nat _ aa (S n) = aa n

iterNat :: (a -> a) -> a -> Nat -> a
iterNat _ a Z = a
iterNat aa a (S n) = iterNat aa (aa a) n

natUnderflow :: String
natUnderflow = "Nat is non-negative"

instance Num Nat where
  (+) = iterNat S
  a * b = iterNat (+a) Z b
  a - Z = a
  Z - b = error natUnderflow
  S a - S b = a - b
  abs = id
  signum Z = Z
  signum (S _) = S Z
  fromInteger x
    | x < 0 = error natUnderflow
    | x == 0 = Z
    | otherwise = S $ fromInteger $ x - 1

instance Enum Nat where
  toEnum x
    | x < 0 = error natUnderflow
    | x == 0 = Z
    | otherwise = S $ toEnum $ x - 1
  fromEnum = iterNat (+1) 0

instance Real Nat where toRational = toRational . fromEnum

instance Integral Nat where
  quotRem a Z = error "divide by zero"
  quotRem a b = until ((< b) . snd) (S *** subtract b) (Z, a)
  divMod = quotRem
  toInteger = iterNat (+1) 0