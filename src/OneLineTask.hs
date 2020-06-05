module OneLineTask where

{-

f x = take 100 [x,x..]

f = take 100 . repeat

f x = replicate 100 x

f = replicate 100

f x = [x..]

f x = const x <$> [0..99]

f x = x <$ [0..99]

f = (>>) [0..99] . pure


f = flip (<$) [0..99]

f x= [ x | _ <- [0..99]]

-}

f=(<$[0..99])
