{-# LANGUAGE LambdaCase #-}
module AlgebraicIsomorphism where

    -- Please copy your code of Isomorphism to here.
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
    isoSymm = (symm, symm)
    
    -- Sometimes, we can treat a Type as a Number:
    -- if a Type t has n distinct value, it's Number is n.
    -- This is formally called cardinality.
    -- See https://en.wikipedia.org/wiki/Cardinality
    
    -- Void has cardinality of 0 (we will abbreviate it Void is 0).
    -- () is 1.
    -- Bool is 2.
    -- Maybe a is 1 + a.
    -- We will be using peano arithmetic so we will write it as S a.
    -- https://en.wikipedia.org/wiki/Peano_axioms
    -- Either a b is a + b.
    -- (a, b) is a * b.
    -- a -> b is b ^ a. Try counting (() -> Bool) and (Bool -> ())
    
    -- Algebraic data type got the name because
    -- it satisfies a lot of algebraic rules under isomorphism
    
    -- a = b -> c = d -> a * c = b * d
    isoProd :: ISO a b -> ISO c d -> ISO (a, c) (b, d)
    isoProd = isoTuple
    
    -- a = b -> c = d -> a + c = b + d
    isoPlus :: ISO a b -> ISO c d -> ISO (Either a c) (Either b d)
    isoPlus = isoEither
    
    -- a = b -> S a = S b
    isoS :: ISO a b -> ISO (Maybe a) (Maybe b)
    isoS = isoMaybe
    
    -- a = b -> c = d -> c ^ a = d ^ b
    isoPow :: ISO a b -> ISO c d -> ISO (a -> c) (b -> d)
    isoPow = isoFunc
    
    -- a + b = b + a
    plusComm :: ISO (Either a b) (Either b a)
    plusComm =
        (\case
            (Left a)  -> Right a
            (Right b) -> Left b,
         \case
            (Left b)  -> Right b
            (Right a) -> Left a
        )
        
    
    -- a + b + c = a + (b + c)
    plusAssoc :: ISO (Either (Either a b) c) (Either a (Either b c))
    plusAssoc =
        (\case
            (Left (Left a)) -> Left a
            (Left (Right b)) -> Right $ Left b
            (Right c) -> Right $ Right c,
         \case
            (Left a) -> Left $ Left a
            (Right (Left b)) -> Left $ Right b
            (Right (Right c)) -> Right c
        )
    
    -- a * b = b * a
    multComm :: ISO (a, b) (b, a)
    multComm = (\(a, b) -> (b, a), \(b, a) -> (a, b))
    
    -- a * b * c = a * (b * c)
    multAssoc :: ISO ((a, b), c) (a, (b, c))
    multAssoc = (\((a, b), c) -> (a, (b, c)), \(a, (b, c)) -> ((a, b), c))
    
    -- dist :: a * (b + c) = a * b + a * c
    dist :: ISO (a, (Either b c)) (Either (a, b) (a, c))
    dist =
        (\case
            (a, Left b)  -> Left (a, b)
            (a, Right c) -> Right (a, c),
         \case
            Left (a, b)  -> (a, Left b)
            Right (a, c) -> (a, Right c)
        )
    
    -- (c ^ b) ^ a = c ^ (a * b)
    curryISO :: ISO (a -> b -> c) ((a, b) -> c)
    curryISO = (uncurry, curry)

    -- 1 = S O (we are using peano arithmetic)
    -- https://en.wikipedia.org/wiki/Peano_axioms
    one :: ISO () (Maybe Void)
    one = (const Nothing, const ())
    
    -- 2 = S (S O)
    two :: ISO Bool (Maybe (Maybe Void))
    two =
      (\case
        True  -> Just Nothing
        False -> Nothing,
       \case
        Nothing        -> False
        (Just Nothing) -> True
      )
    
    -- O + b = b
    plusO :: ISO (Either Void b) b
    plusO = (left, Right)
      where
        left (Left  x) = absurd x -- absurd :: Void -> a
        left (Right x) = x
    
    -- S a + b = S (a + b)

    -- 0 + b = b
    --   S a = S a
    --     b = S b

    plusS :: ISO (Either (Maybe a) b) (Maybe (Either a b))
    plusS =
      (\case
        (Left Nothing) -> Nothing
        (Left (Just a)) -> Just $ Left a
        (Right b) -> Just $ Right b,
       \case
        Nothing -> Left Nothing
        Just (Left a) -> Left $ Just a
        Just (Right b) -> Right b
      )
    
    -- 1 + b = S b
    plusSO :: ISO (Either () b) (Maybe b)
    plusSO = isoPlus one refl `trans` -- lhs 1 + b = (S O) + b
              plusS `trans`           -- >>> S a + b = S (a + b)
              isoS plusO              -- rhs 0 + b + 1 = b + 1
                                      -- (S O) + b = b + 1
    
    -- O * a = O
    multO :: ISO (Void, a) Void
    multO = (f, g)
      where
        f = fst
        g x = (x, absurd x)

    
    -- S a * b = b + a * b

    -- a = Zero
    -- (0 - 1) * b = 1 + (0 * 1)
    --    S(0) * b = b + (0 * b)
    --             = b + 0
    --             = b

    multS :: ISO (Maybe a, b) (Either b (a, b))
    multS =
      (\case
        (Nothing, b) -> Left b
        (Just a , b) -> Right (a, b),
       \case
        (Left b)       -> (Nothing, b)
        (Right (a, b)) -> (Just a, b)
      )
    
    -- 1 * b = b
    multSO :: ISO ((), b) b
    multSO =
      isoProd one refl `trans`      -- lhs 1 * b = (S O) * b
        multS `trans`               -- >>> S a * b = b + a * b
        isoPlus refl multO `trans`  -- rhs 0 * a + b = b * 0
        plusComm `trans`            -- rhs     0 + b = b + 0
        plusO                       -- rhs     0 + b = b
    
    -- a ^ O = 1
    powO :: ISO (Void -> a) ()
    powO = (const (), const absurd)
    
    -- a ^ (S b) = a * (a ^ b)

    -- a ^ 0 = a * (a ^ (0 - 1)) :: ISO (Maybe 0 -> a) (a, 0 -> a)
    --       = a * (a ^ (-1))    :: ISO (Maybe 0 -> a) (a, Maybe 0 -> a)

    -- a ^ 2 = a * (a ^ (2 - 1)) :: ISO (Maybe 2 -> a) (a, 2 -> a)
    --       = a * a             :: ISO (Maybe 2 -> a) (a, a)
    --       = a ^ 2

    powS :: ISO (Maybe b -> a) (a, b -> a)
    powS = (f, g)
      where
        f mba = (mba Nothing, mba . Just)
        g (a, ba) mb = case mb of
          Nothing  -> a
          (Just b) -> ba b
        -- f mba = let ba = mba . Just in (a, ba)
        -- g (a, ba) = const a
    
    -- a ^ 1 = a
    -- Go the hard way (like multSO, plusSO)
    -- to prove that you really get what is going on!
    powSO :: ISO (() -> a) a
    powSO =
      isoPow one refl `trans`       -- lhs a ^ 1 = a ^ (S O)
        powS `trans`                -- >>> a ^ (S O) = a * a ^ (S O)
        isoProd refl powO `trans`   -- rhs a * (? ^ 0) = a * 1  isoProd refl powO
        multComm `trans`            -- rhs a * 1
        multSO                      -- rhs a * 1 = a
    -- Here's a trick: 
    -- replace undefined with the rhs of the comment on previous line
    -- When you're not sure what to fill in for a value,
    -- Have it as a _
    -- GHC will goes like
    -- "Found hole `_' with type: ISO (() -> a) (Maybe b0 -> a0)"
    -- So you can immediately see value of what type are needed
    -- This process can be repeat indefinitely -
    -- For example you might replace `_` with `isoFunc _ _`
    -- So GHC hint you on more specific type.
    -- This is especially usefull if you have complex type.
    -- See https://wiki.haskell.org/GHC/Typed_holes
    -- And "stepwise refinement" for more details.