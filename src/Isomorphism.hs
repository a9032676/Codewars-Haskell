{-# LANGUAGE LambdaCase #-}
module Isomorphism where

    import Data.Void
    import Data.Maybe
    -- A type of `Void` have no value.
    -- So it is impossible to construct `Void`,
    -- unless using undefined, error, unsafeCoerce, infinite recursion, etc
    -- And there is a function
    -- absurd :: Void -> a
    -- That get any value out of `Void`
    -- We can do this becuase we can never have void in the zeroth place.
    
    -- so, when are two type, `a` and `b`, considered equal?
    -- a definition might be, it is possible to go from `a` to `b`,
    -- and from `b` to `a`.
    -- Going a roundway trip should leave you the same value.
    -- Unfortunately it is virtually impossible to test this in Haskell.
    -- This is called Isomorphism.
    
    type ISO a b = (a -> b, b -> a)
    
    -- given ISO a b, we can go from a to b
    substL :: ISO a b -> (a -> b)
    substL = fst
    
    -- and vice versa
    substR :: ISO a b -> (b -> a)
    substR = snd
    
    -- There can be more than one ISO a b
    isoBool :: ISO Bool Bool
    isoBool = (id, id)
    
    isoBoolNot :: ISO Bool Bool
    isoBoolNot = (not, not)
    
    -- isomorphism is reflexive
    refl :: ISO a a
    refl = (id, id)
    
    -- isomorphism is symmetric
    symm :: ISO a b -> ISO b a
    symm (ab, ba) = (ba, ab)
    
    -- isomorphism is transitive
    trans :: ISO a b -> ISO b c -> ISO a c
    trans (ab, ba) (bc, cb) = (bc . ab, ba . cb)
    
    -- (a, c) -> (b, d)
    -- (b, d) -> (a, c)
    -- We can combine isomorphism:
    isoTuple :: ISO a b -> ISO c d -> ISO (a, c) (b, d)
    isoTuple (ab, ba) (cd, dc) = (\(a, c) -> (ab a, cd c), \(b, d) -> (ba b, dc d))
    
    -- [a] -> [b]
    -- [b] -> [a]
    isoList :: ISO a b -> ISO [a] [b]
    isoList (ab, ba) = (map ab, map ba)
    
    isoMaybe :: ISO a b -> ISO (Maybe a) (Maybe b)
    isoMaybe (ab, ba) = (fmap ab, fmap ba)
    
    -- (Either a c) -> (Either b d)
    -- (Either b d) -> (Either a c)
    isoEither :: ISO a b -> ISO c d -> ISO (Either a c) (Either b d)
    isoEither (ab, ba) (cd, dc) =
      (\case
        (Left a)  -> Left (ab a)
        (Right c) -> Right (cd c),
       \case
        (Left b)  -> Left (ba b)
        (Right d) -> Right (dc d)
      )
    
    -- ISO (a -> c) (b -> d)
    -- (a -> c) <-> (b -> d)
    -- (a -> c)  -> (b -> d)
    -- (b -> d)  -> (a -> c)
    isoFunc :: ISO a b -> ISO c d -> ISO (a -> c) (b -> d)
    isoFunc (ab, ba) (cd, dc) = (\ac -> cd . ac . ba, \bd -> dc . bd . ab)
    
    -- ISO (Maybe a) (Maybe b)

    -- a -> b
    -- a -> Just a -> Just bf

    -- b -> a

    -- Maybe a -> Maybe b
    --   Nothing -> Just b
    --   Just a  -> Just b

    -- Maybe b -> Maybe a
    --   Nothing -> Just a
    --   Just b  -> Just a

    -- ((Maybe a) -> (Maybe b)) -> (a -> b)
    -- ((Maybe b) -> (Maybe a)) -> (b -> a)
    -- Going another way is hard (and is generally impossible)
    isoUnMaybe :: ISO (Maybe a) (Maybe b) -> ISO a b
    isoUnMaybe (to, from) =
      (\a -> case to $ Just a of
        Nothing  -> case to Nothing of
                      Nothing  -> error "QwQ"
                      (Just b) -> b
        (Just b) -> b,
       \b -> case from $ Just b of
        Nothing  -> case from Nothing of
                      Nothing  -> error "QwQ"
                      (Just a) -> a
        (Just a) -> a
      )
    -- Remember, for all valid ISO, converting and converting back
    -- Is the same as the original value.
    -- You need this to prove some case are impossible.
    
    -- We cannot have
    -- isoUnEither :: ISO (Either a b) (Either c d) -> ISO a c -> ISO b d.
    -- Note that we have

    -- Either [()] () <-> Either [()] Void
    isoEU :: ISO (Either [()] ()) (Either [()] Void)
    isoEU = (\case
                (Left xs) -> Left $ () : xs
                (Right ()) -> Left [],
             \case
                (Left (_:xs)) -> Left xs
                (Left []) -> Right ()
            )
      
    -- where (), the empty tuple, has 1 value, and Void has 0 value
    -- If we have isoUnEither,
    -- We have ISO () Void by calling isoUnEither isoEU
    -- That is impossible, since we can get a Void by substL on ISO () Void
    -- So it is impossible to have isoUnEither
    
    -- And we have isomorphism on isomorphism!
    isoSymm :: ISO (ISO a b) (ISO b a)
    isoSymm = (\isoL -> (substR isoL, substL isoL) , \isoR -> (substR isoR, substL isoR))