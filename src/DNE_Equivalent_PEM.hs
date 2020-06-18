{-# LANGUAGE RankNTypes #-}

module DNE_Equivalent_PEM where

import Prelude
import Data.Void

-- Principle of Excluded Middle
type AxiomPEM = forall a. forall b. (a -> b) -> ((a -> Void) -> b) -> b

-- Double Negation Elimination
-- ¬¬P → P
type AxiomDNE = forall a. ((a -> Void) -> Void) -> a

-- dne :: forall a. forall b. (a -> b) -> ((a -> Void) -> b) -> b
-- f   :: a -> b
-- g   :: (a -> Void) -> b
-- h   :: b -> Void
-- a   :: a

from :: AxiomDNE -> AxiomPEM
from dne f g = dne $ \h -> h $ f $ absurd $ h $ g $ h . f

to :: AxiomPEM -> AxiomDNE
to pem f = pem id $ absurd . f