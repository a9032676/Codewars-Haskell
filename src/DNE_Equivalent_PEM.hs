{-# LANGUAGE RankNTypes #-}

module DNE_Equivalent_PEM where

import Prelude
import Data.Void
import Control.Monad.Fix

-- Principle of Excluded Middle
type AxiomPEM = forall a. forall b. (a -> b) -> ((a -> Void) -> b) -> b

-- Double Negation Elimination
-- ¬¬P → P
type AxiomDNE = forall a. ((a -> Void) -> Void) -> a

-- dne :: ((a -> Void) -> Void) -> a
-- f   :: a -> b
-- g   :: (a -> Void) -> b


-- Deduction. AxiomDNE
-- ((b -> Void) -> Void) -> b
-- - (b -> Void) -> Void        1. hypothesis
--

-- Deduction. AxiomPEM
--    (a -> b   ) -> ((a -> Void) -> b   ) -> b
-- := (a -> Void) -> ((a -> Void) -> Void) -> Void
-- - (a -> Void)                                    1. hypothesis
-- - - (a -> Void) -> Void                          2. hypothesis
-- - - Void -> Void                                 3. modus ponens 2,1
-- - ((a -> Void) -> Void) -> Void                  4. deduction from 2 to 3
-- (a -> Void) -> ((a -> Void) -> Void) -> Void     5. deduction from 1 to 4. QED.

-- Deduction. AxiomDNE -> AxiomPEM
-- (((b -> Void) -> Void) -> b) -> (a -> b) -> ((a -> Void) -> b) -> b
-- - ((b -> Void) -> Void) -> b 1. hypothesis
-- - - a -> b                   2. hypothesis
-- - - - (a -> Void) -> b       3. hypothesis


-- (((b -> Void) -> Void) -> b) -> (a -> b) -> ((a -> Void) -> b) -> b
from :: AxiomDNE -> AxiomPEM
from dne f g = g $ \a -> undefined

-- (forall a. forall b. (a -> b) -> ((a -> Void) -> b) -> b) -> forall a. ((a -> Void) -> Void) -> a
to :: AxiomPEM -> AxiomDNE
to pem = absurd $ pem id fix