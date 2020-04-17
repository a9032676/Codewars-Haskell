{-# LANGUAGE TemplateHaskell #-}
module TupleMaker (tuple) where

import Language.Haskell.TH

-- | Creates a lambda that takes `n` arguments and
-- | returns an n-tuple of those arguments.
tuple :: Int -> Q Exp
tuple x = [| \y -> y + x + 1 |]
