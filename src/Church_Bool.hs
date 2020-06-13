{-# Language RankNTypes #-}
module Church_Bool where

import Prelude hiding (Bool,False,True,not,and,or,(&&),(||),(==),(/=))

type Boolean = forall a. a -> a -> a -- this requires RankNTypes

false,true :: Boolean
true  = const
false = not true

not :: Boolean -> Boolean
and,or,xor :: Boolean -> Boolean -> Boolean

not a   = a false true
and a b = a b false
or  a b = a (b true true) b
xor a b = a (not b) b