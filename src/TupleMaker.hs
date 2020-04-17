{-# LANGUAGE TemplateHaskell #-}
module TupleMaker (tuple) where

import Language.Haskell.TH
import Control.Monad

-- | Creates a lambda that takes `n` arguments and
-- | returns an n-tuple of those arguments.

tuple :: Int -> Q Exp
tuple n = do
    ps <- replicateM n $ newName "x"
    return $ LamE (VarP <$> ps) $ TupE $ VarE <$> ps

-- tuple' :: Int -> Q Exp
-- tuple' x = f x []
--     where
--         f 0 r = return $ LamE (VarP <$> r) $ TupE $ VarE <$> r
--         f n r = f (n-1) $ r ++ [mkName ("x" ++ show n)]