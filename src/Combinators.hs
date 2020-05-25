{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
module Combinators where
-- import PredefinedCombinators (SKI(..))

-- Formally, in lambda calculus, SKI combinators are defined as:
-- Sxyz = xz(yz)
-- Kxy = x
-- Ix = x
-- Where every term is applied to the previous term left-associatively, e.g abcd = ((ab)c)d.
-- (Note that I can be defined in terms of S and K. It's usually kept mostly for convenience.)
-- 
-- I and K should be familiar to you as `id` and `const` in Haskell, but S is a bit complicated.
-- Formally, S represents term application: it accepts 3 arguments `x`, `y`, `z`,
-- which then we apply to `x` two arguments: first `z`, then `yz`, i.e the result of `z` applied to `y`.
-- (Everything in lambda calculus are terms. There are no distinctions between values and functions.)
-- 
-- Sure, you can use SKI combinators in Haskell all you want, but don't forget that this is Haskell ;-)
-- With a little effort (a.k.a GADT), SKI can be encoded as an AST in Haskell.
-- Encoding SKI as an AST allows the typechecker to verify AST type correctness for us.
-- (It also prevents us from using non-SKI things to cheat the system ;-))
-- Note that we also need `Ap`, which applies one AST to another at the type level,
-- to provide ourselves a way to transform our AST.

------------------------------------------------------------------------------
-- Task #1: Read SKI's data type (pre)defined as below, and understand what's going on ;-)


data SKI :: * -> * where
  Ap :: SKI (a -> b) -> SKI a -> SKI b
  S :: SKI ((a -> b -> c) -> (a -> b) -> a -> c)
  K :: SKI (a -> b -> a)
  I :: SKI (a -> a)



-- If we also have:  Var :: a -> SKI a , then it's just a normal DSL.
-- It will also automatically give us the Functor and Applicative instances.
-- However, the point of combinator calculus (or lambda calculus in general)
-- is to operate on combinators and terms themselves, and so there are no ways
-- (and will never have any) to inject values directly.
-- 
-- If there are no ways to inject values directly, then how do we do things with them?
-- The answer is simple: the combinators change how a term is applied to its arguments.
-- This allows us to express any lambda term in terms of these combinators.
-- This is Church's thesis: Any suitable combinator basis (e.g SK/SKI) can form all kinds (as in Haskell's Kind) of computable functions.

------------------------------------------------------------------------------
-- Task #2: implement the evaluator and pretty-printer for the SKI system.

evalSKI :: SKI a -> a
evalSKI (Ap f a) = evalSKI f $ evalSKI a
evalSKI S = \x y z -> x z (y z)
evalSKI K = const
evalSKI I = id


-- The pretty-printer should follow this format:
-- I, K, S -> "I", "K", "S"
-- Ap a b -> "(a b)" where a and b are subterms
prettyPrintSKI :: SKI a -> String
prettyPrintSKI (Ap f a) = "(" ++ prettyPrintSKI f ++ " " ++ prettyPrintSKI a ++ ")"
prettyPrintSKI S = "S"
prettyPrintSKI K = "K"
prettyPrintSKI I = "I"


------------------------------------------------------------------------------
-- Task #3: write the following basic combinators in the SKI system.

-- Transforming a given lambda term to a combination of combinators is basically making
-- your code point-free in Haskell: You write \x y -> x (lambda function) into `const` (point-free).
-- To distinguish combinators (sort-of functions) from parameters (what we want to get rid of eventually),
-- we use uppercase letters for combinators and lowercase letters for parameters(and whitespace to separate each term).
-- In order to do this, we can add or remove extra parameters *at the end* of an expression (Eta reduction).

-- e.g: By definition,
--    I
-- => \x. x          -- apply I
-- => \x. K x _      -- replace x with K x _
-- => \x. K x (_ x)  -- substitute _ with _ x (_ is just a dummy term which can be anything)
-- => \x. S K _ x    -- condense S
-- => S K _          -- eta reduction
-- QED

-- The proof goes to the other way too (all operations above are bijective),
-- i.e this inverse of the above proof automatically true too, as below:
--    S K _
-- => \x. S K _ x    -- eta reduction
-- => \x. K x (_ x)  -- apply S
-- => \x. x          -- apply K
-- => I              -- condense I
-- QED

-- Hence this gives us the proof that S K _ = I, i.e I can be expressed by S and K.

-- After finishing our hand-written proof, by Curry-Howard correspondence,
-- we can always encode our initial lambda term into a type (as long as no recursion (e.g Y) happens).
-- This allows us to check for proof correctness via type-checker. How convenient!

-- As can be seen above, expanding everything into S and K only is long and tedious.
-- Any simple lambda might give rise to a proof of dozen of lines:
--    \x. \y. \z. x z y
-- => \x. \y. \z. (x z) (K y z)   -- replace y with K y z
-- => \x. \y. \z. (x z) ((K y) z)
-- => \x. \y. \z. S x (K y) z     -- condense S
-- => \x. \y. S x (K y)           -- eta reduction
-- => \x. \y. (S x) (K y)
-- => \x. \y. (K (S x) y) (K y)   -- replace S x with K (S x) y
-- => \x. \y. (K (S x)) y (K y)
-- => \x. \y. S (K (S x)) K y     -- condense S
-- => \x. S (K (S x)) K           -- eta reduction
-- ...

-- In fact, there are infinitely many ways to express any lambda term,
-- with no computationally easy way to find the shortest one.
-- That's why we instead define lots of other combinators to help us compose new combinators
-- without expanding into a long chain of S and K (which are known to grow non-linearly).
-- You should do that. Break a proof into parts to reduce the mental workload.

-- rev :: a -> (a -> b) -> b
-- rev a b = b a

--    \a. \b. b a
-- => \a. \b. I b a
-- => \a. \b. (I b) a
-- => \a. \b. (I b) (K a b)
-- => \a. \b. I b (K a b)
-- => \a. \b. I b ((K a) b)
-- => \a. \b. S I (K a) b
-- => \a. S I (K a)
-- => \a. (S I) (K a)
-- => \a. (K (S I) a) (K a)
-- => S (K (S I)) K
-- => (S (K (S I)) K)

rev :: SKI (a -> (a -> b) -> b)
rev = Ap (Ap S (Ap K (Ap S I))) K

-- comp :: (b -> c) -> (a -> b) -> (a -> c)
-- comp f g x = f (g x)

--    \f. \g. \x. f (g x)
-- => \f. \g. \x. (K f x) (g x)
-- => \f. \g. \x. (K f) x (g x)
-- => \f. \g. \x. S (K f) g x
-- => \f. \g. S (K f) g
-- => \f. S (K f)
-- => \f. (K S f) (K f)
-- => \f. S (K S) K f
-- => S (K S) K
-- => (S (K S)) K
-- => ((S (K S)) K)

-- B-Combinator
-- B x y z = x (y z)
comp :: SKI ((b -> c) -> (a -> b) -> (a -> c))
comp = Ap (Ap S (Ap K S)) K

-- flip :: (a -> b -> c) -> (b -> a -> c)
-- flip f y x = f x y

-- Normal Derivation:
--    \f. \y. \x. f x y
-- => \f. \y. \x. (f x) y
-- => \f. \y. S (\x. f x) (\x. y)
-- => \f. \y. S f (\x. y)
-- => \f. \y. S f (K y)
-- => \f. \y. (S f) (K y)
-- => \f. S (\y. S f) (\y. K y)
-- => \f. S (\y. S f) K
-- => \f. S (S (\y. S) (\y. f)) K
-- => \f. S (S (K S) (K f)) K
-- => S (\f. S (S (K S) (K f))) (\f. K)
-- => S (\f. S (S (K S) (K f))) (K K)
-- => S (S (\f. S) (\f. S (K S) (K f))) (K K)
-- => S (S (K S) (\f. S (K S) (K f))) (K K)
-- => S (S (K S) (S (\f. S (K S)) (\f. K f))) (K K)
-- => S (S (K S) (S (\f. S (K S)) K)) (K K)
-- => S (S (K S) (S (S (\f. S) (\f. K S)) K)) (K K)
-- => S (S (K S) (S (S (K S) (\f. K S)) K)) (K K)
-- => S (S (K S) (S (S (K S) (S (\f. K) (\f. S))) K)) (K K)
-- => S (S (K S) (S (S (K S) (S (K K) (K S))) K)) (K K)
-- => (S (S (K S) (S (S (K S) (S (K K) (K S))) K))) (K K)

-- Use BCKW System:
--    \f. \y. \x. f x y
-- => \f. \y. S (\x. f x) (\x. y)
-- => \f. \y. S f (\x. y)
-- => \f. \y. S f (K yλ)
-- => \f. \y. B (S f) K y
-- => \f. B (S f) K
-- => \f. B B S f K
-- => S (\f. B B S f) (\f. K)
-- => S (\f. B B S f) (K K)
-- => S (B B S) (K K)
--          or
-- => B (S (B B S)) K K

-- C-Combinator
-- C x y z = x z y
flip' :: SKI ((a -> b -> c) -> (b -> a -> c))
flip' = Ap (Ap S (Ap (Ap comp comp) S)) (Ap K K)
-- flip' = Ap (Ap (Ap comp (Ap S (Ap (Ap comp comp) S))) K) K
-- flip' = Ap (Ap S (Ap (Ap S (Ap K S)) (Ap (Ap S (Ap (Ap S (Ap K S)) (Ap (Ap S (Ap K K)) (Ap K S)))) K))) (Ap K K)

-- rotr :: a -> (c -> a -> b) -> c -> b
-- rotr x y z = y z x

-- Normal Derivation:
--    \x. \f. \y. f y x
-- => \x. \f. S (\y. f y) (\y. x)
-- => \x. \f. S f (\y. x)
-- => \x. \f. S f (K x)
-- => \x. S (\f. S f) (\f. K x)
-- => \x. S S (\f. K x)
-- => \x. S S (K (K x))
-- => S (\x. S S) (\x. K (K x))
-- => S (S (\x. S) (\x. S)) (\x. K (K x))
-- => S (S (K S) (K S)) (\x. K (K x))
-- => S (S (K S) (K S)) (S (\x. K) (\x. K x))
-- => S (S (K S) (K S)) (S (\x. K) K)
-- => S (S (K S) (K S)) (S (K K) K)

-- Use BCKW System:
--    \x. \f. \y. f y x
-- => \x. \f. \y. C f x y
-- => \x. \f. C f x
-- => \x. \f. C C x f
-- => \x. C C x
-- => C C
rotr :: SKI (a -> (c -> a -> b) -> c -> b)
rotr = Ap flip' flip'
-- rotr = Ap (Ap S (Ap (Ap S (Ap K S)) (Ap K S))) (Ap (Ap S (Ap K K)) K)

-- rotv :: a -> b -> (a -> b -> c) -> c
-- rotv x y z = z x y

-- Normal Derivation:
--    \x. \y. \f. f x y
-- => \x. \y. S (\f. f x) (\f. y)
-- => \x. \y. S (S (\f. f) (\f. x)) (K y)
-- => \x. \y. S (S I (K x)) (K y)
-- => \x. S (\y. S (S I (K x))) (\y. K y)
-- => \x. S (\y. S (S I (K x))) K
-- => \x. S (S (\y. S) (\y. S I (K x))) K
-- => \x. S (S (K S) (\y. S I (K x))) K
-- => \x. S (S (K S) (S (\y. S I) (\y. K x))) K
-- => \x. S (S (K S) (S (S (\y. S) (\y. I)) (\y. K x))) K
-- => \x. S (S (K S) (S (S (K S) (K I)) (\y. K x))) K
-- => \x. S (S (K S) (S (S (K S) (K I)) (S (\y. K) (\y. x)))) K
-- => \x. S (S (K S) (S (S (K S) (K I)) (S (K K) (K x)))) K
-- => S (\x. S (S (K S) (S (S (K S) (K I)) (S (K K) (K x))))) (\x. K)
-- => S (\x. S (S (K S) (S (S (K S) (K I)) (S (K K) (K x))))) (K K)
-- => S (S (\x. S) (\x. S (K S) (S (S (K S) (K I)) (S (K K) (K x))))) (K K)
-- => S (S (K S) (\x. S (K S) (S (S (K S) (K I)) (S (K K) (K x))))) (K K)
-- => S (S (K S) (S (\x. S (K S)) (\x. S (S (K S) (K I)) (S (K K) (K x))))) (K K)
-- => S (S (K S) (S (S (\x. S) (\x. K S)) (\x. S (S (K S) (K I)) (S (K K) (K x))))) (K K)
-- => S (S (K S) (S (S (K S) (\x. K S)) (\x. S (S (K S) (K I)) (S (K K) (K x))))) (K K)
-- => S (S (K S) (S (S (K S) (S (\x. K) (\x. S))) (\x. S (S (K S) (K I)) (S (K K) (K x))))) (K K)
-- => S (S (K S) (S (S (K S) (S (K K) (K S))) (\x. S (S (K S) (K I)) (S (K K) (K x))))) (K K)
-- => S (S (K S) (S (S (K S) (S (K K) (K S))) (S (\x. S (S (K S) (K I))) (\x. S (K K) (K x))))) (K K)
-- => S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (\x. S) (\x. S (K S) (K I))) (\x. S (K K) (K x))))) (K K)
-- => S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (\x. S (K S) (K I))) (\x. S (K K) (K x))))) (K K)
-- => S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (\x. S (K S)) (\x. K I))) (\x. S (K K) (K x))))) (K K)
-- => S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (S (\x. S) (\x. K S)) (\x. K I))) (\x. S (K K) (K x))))) (K K)
-- => S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (S (K S) (\x. K S)) (\x. K I))) (\x. S (K K) (K x))))) (K K)
-- => S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (S (K S) (S (\x. K) (\x. S))) (\x. K I))) (\x. S (K K) (K x))))) (K K)
-- => S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (S (K S) (S (K K) (K S))) (\x. K I))) (\x. S (K K) (K x))))) (K K)
-- => S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (S (K S) (S (K K) (K S))) (S (\x. K) (\x. I)))) (\x. S (K K) (K x))))) (K K)
-- => S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (S (K S) (S (K K) (K S))) (S (K K) (K I)))) (\x. S (K K) (K x))))) (K K)
-- => S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (S (K S) (S (K K) (K S))) (S (K K) (K I)))) (S (\x. S (K K)) (\x. K x))))) (K K)
-- => S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (S (K S) (S (K K) (K S))) (S (K K) (K I)))) (S (\x. S (K K)) K)))) (K K)
-- => S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (S (K S) (S (K K) (K S))) (S (K K) (K I)))) (S (S (\x. S) (\x. K K)) K)))) (K K)
-- => S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (S (K S) (S (K K) (K S))) (S (K K) (K I)))) (S (S (K S) (S (\x. K) (\x. K))) K)))) (K K)
-- => S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (S (K S) (S (K K) (K S))) (S (K K) (K I)))) (S (S (K S) (S (K K) (K K))) K)))) (K K)

-- Use BCKW System + SKI + Custom combinator:
-- R := rev
--    \x. \y. \f. f x y
-- => \x. \y. S (\f. f x) (\f. y)
-- => \x. \y. S (\f. f x) (K y)
-- => \x. \y. S (\f. R x f) (K y)
-- => \x. \y. S (R x) (K y)
-- => \x. \y. B (S (R x)) K y
-- => \x. B (S (R x)) K
-- => S (\x. B (S (R x))) (\x. K)
-- => S (\x. B (S (R x))) (K K)
-- => S (S (\x. B) (\x. S (R x))) (K K)
-- => S (S (K B) (\x. S (R x))) (K K)
-- => S (S (K B) (\x. B S R x)) (K K)
-- => S (S (K B) (B S R)) (K K)

rotv :: SKI (a -> b -> (a -> b -> c) -> c)
rotv = Ap (Ap S (Ap (Ap S (Ap K comp)) (Ap (Ap comp S) rev))) (Ap K K)
-- rotv = Ap (Ap S (Ap (Ap S (Ap K S)) (Ap (Ap S (Ap (Ap S (Ap K S)) (Ap (Ap S (Ap K K)) (Ap K S)))) (Ap (Ap S (Ap (Ap S (Ap K S)) (Ap (Ap S (Ap (Ap S (Ap K S)) (Ap (Ap S (Ap K K)) (Ap K S)))) (Ap (Ap S (Ap K K)) (Ap K I))))) (Ap (Ap S (Ap (Ap S (Ap K S)) (Ap (Ap S (Ap K K)) (Ap K K)))) K))))) (Ap K K)

-- Y-Combinator
-- join :: (a -> a -> b) -> a -> b
-- join f x = f x x

--    \f. \x. f x x
-- => \f. S (\x. f x) (\x. x)
-- => \f. S f I
-- => S (\f. S f) (\f. I)
-- => S S (K I)
-- =>     or
-- => B (S S) K I

-- We can't write `fix` i.e Y in Haskell because Haskell is typed
-- (well, at least without recursive types), but we can still write `join`
join :: SKI ((a -> a -> b) -> a -> b)
join = Ap (Ap S S) (Ap K I)

------------------------------------------------------------------------------
-- Task #4: implement Boolean algebra in the SKI system

-- Boolean algebra is represented as an if-else statement:
-- T accepts two arguments, and returns the first argument.
-- F also accepts two arguments, but returns the second instead.
--q
-- Note: all the operators should be prefix. They should also be lazy,
-- which should come along naturally if you're doing it correctly.

-- type synonym to help reduce clutter in type definition
type Bool' a = a -> a -> a

-- Note: The correct type of everything down there should be something along
-- (forall a. Bool a) instead of (Bool a)
-- However, we cannot express it in SKI because it is embedded here as
-- simply typed lambda calculus (STLC), which does not have enough expressive power for this.
-- (In other systems, e.g untyped lambda calculus (UTLC) and System F, we don't have this problem.)
-- As a result, your term can be correct even when the type-checking cannot deduce such.
-- If you're absolutely ensure that your proof is correct, you can remove the type annotation,
-- or rewrite its corresponding SKI type.

-- true :: a -> a -> a
-- true x y = x

--    \x. \y. x
-- => K

true :: SKI (Bool' a)
true = K

-- false :: a -> a -> a
-- false x y = y

--    \x. \y. y
-- => \x. I
-- => K I

false :: SKI (Bool' a)
false = Ap K I

-- not :: (a -> a -> a) -> (a -> a -> a)
-- not f x y = f y x

--    \f. \x. \y. f y x
-- => \f. \x. S (\y. f y) (\y. x)
-- => \f. \x. S f (\y. x)
-- => \f. \x. S f (K x)
-- => \f. S (\x. S f) (\x. K x)
-- => \f. S (\x. S f) K
-- => \f. S (S (\x. S) (\x. f)) K
-- => \f. S (S (K S) (K f)) K
-- => S (\f. S (S (K S) (K f))) (\f. K)
-- => S (\f. S (S (K S) (K f))) (K K)
-- => S (S (\f. S) (\f. S (K S) (K f))) (K K)
-- => S (S (K S) (\f. S (K S) (K f))) (K K)
-- => S (S (K S) (S (\f. S (K S)) (\f. K f))) (K K)
-- => S (S (K S) (S (\f. S (K S)) K)) (K K)
-- => S (S (K S) (S (S (\f. S) (\f. K S)) K)) (K K)
-- => S (S (K S) (S (S (K S) (\f. K S)) K)) (K K)
-- => S (S (K S) (S (S (K S) (S (\f. K) (\f. S))) K)) (K K)
-- => S (S (K S) (S (S (K S) (S (K K) (K S))) K)) (K K)

not' :: SKI (Bool' a -> Bool' a)
not' = flip'
-- not' = Ap (Ap S (Ap (Ap S (Ap K S)) (Ap (Ap S (Ap (Ap S (Ap K S)) (Ap (Ap S (Ap K K)) (Ap K S)))) K))) (Ap K K)

-- and :: Bool' (Bool' a) -> Bool' a -> Bool' a
-- and :: (forall a. Bool' a) -> Bool' a -> Bool' a
-- and p q = p q (p (\x y -> x) (\x y -> y))

-- Normal Derivation:
--    \p. \q. p q p
-- => \p. S (\q. p q) (\q. p)
-- => \p. S p (\q. p)
-- => \p. S p (K p)
-- => S (\p. S p) (\p. K p)
-- => S S K

-- BCKW System + SKI Combinators + Custom Combinators
--    \p. \q. p q p
-- => \p. \q. C p p q
-- => \p. C p p
-- => \p. C p p
-- => S (\p. C p) (\p. p)
-- => S C I

-- But in Haskell type system, term p own this type (Without forall): ((a -> a -> a) -> (a -> a -> a) -> (a -> a -> a))
-- So terms [p q p] must be change to something like [p q (p _ _)]

-- Rv := rotv
-- Rr := rotr

--    \p. \q. p q (p T F)
-- => \p. \q. Rv (p T F) p q
-- => \p. Rv (p T F) p
-- => S (\p. Rv (p T F)) (\p. p)
-- => S (\p. Rv (p T F)) I
-- => S (S (\p. Rv) (\p. p T F)) I
-- => S (S (K Rv) (\p. p T F)) I    [p must be change to p T F]
-- => S (S (K Rv) (\p. p T F)) I
-- => S (S (K Rv) (\p. Rr T F p)) I
-- => S (S (K Rv) (Rr T F)) I

-- T T T
-- T F F
-- T T F
-- F F F

-- and := \x. \y. x (y T F) F
-- and := \x. \y. x y F

--    \x. \y. x y F
-- => \x. \y. Rr F x y
-- => \x. Rr F x
-- => Rr F

and' :: SKI (Bool' (Bool' a) -> Bool' a -> Bool' a)
and' = Ap rotr false

--    \p. \q. p p q
-- => \p. S (\q. p p) (\q. q)
-- => \p. S (\q. p p) I
-- => \p. S (S (\q. p) (\q. p)) I
-- => \p. S (S (K p) (K p)) I
-- => S (\p. S (S (K p) (K p))) (\p. I)
-- => S (\p. S (S (K p) (K p))) (K I)
-- => S (S (\p. S) (\p. S (K p) (K p))) (K I)
-- => S (S (K S) (\p. S (K p) (K p))) (K I)
-- => S (S (K S) (S (\p. S (K p)) (\p. K p))) (K I)
-- => S (S (K S) (S (\p. S (K p)) K)) (K I)
-- => S (S (K S) (S (S (\p. S) (\p. K p)) K)) (K I)
-- => S (S (K S) (S (S (K S) (\p. K p)) K)) (K I)
-- => S (S (K S) (S (S (K S) K) K)) (K I)

-- or := \x. \y. x (y T T) (y T F)
-- or := \x. \y. x (y T T) y
-- or := \x. \y. x T y

-- R := rev
--    \x. \y. x T y
-- => \x. x T
-- => \x. R T x
-- => R T

or' :: SKI (Bool' (Bool' a) -> Bool' a -> Bool' a)
or' = Ap rev true
-- or' = Ap (Ap S (Ap (Ap S (Ap K S)) (Ap (Ap S (Ap (Ap S (Ap K S)) K)) K))) (Ap K I)

-- N := not
-- xor := \x. \y. x (y F T) (y T F)
-- xor := \x. \y. x N (y T F)
-- xor := \x. \y. x N T

-- Normal Derivation (Infinite type error)
--    \x. \y. x N T
-- => \x. S (\y. x N) (\y. T)
-- => \x. S (\y. x N) (K T)
-- => \x. S (S (\y. x) (\y. N)) (K T)
-- => \x. S (S (K x) (K N)) (K T)
-- => S (\x. S (S (K x) (K N))) (\x. K T)
-- => S (\x. S (S (K x) (K N))) (S (K K) (K T))
-- => S (S (\x. S) (\x. S (K x) (K N))) (S (K K) (K T))
-- => S (S (K S) (\x. S (K x) (K N))) (S (K K) (K T))
-- => S (S (K S) (S (\x. S (K x)) (\x. K N))) (S (K K) (K T))
-- => S (S (K S) (S (\x. S (K x)) (S (K K) (K N)))) (S (K K) (K T))
-- => S (S (K S) (S (\x. B S K x) (S (K K) (K N)))) (S (K K) (K T))
-- => S (S (K S) (S (B S K) (S (K K) (K N)))) (S (K K) (K T))

-- Rv := rotv
--    \x. \y. x (y F T) (y T F)
-- => \x. \y. x (Rv F T y) (Rv T F y)
-- => \x. S (\y. x (Rv F T y)) (\y. Rv T F y)
-- => \x. S (\y. x (Rv F T y)) (Rv T F)
-- => \x. S (S (\y. x) (\y. Rv F T y)) (Rv T F)
-- => \x. S (S (K x) (Rv F T)) (Rv T F)
-- => S (\x. S (S (K x) (Rv F T))) (\x. Rv T F)
-- => S (\x. S (S (K x) (Rv F T))) (S (\x. Rv T) (\x. F))
-- => S (\x. S (S (K x) (Rv F T))) (S (S (K Rv) (K T)) (K F))
-- => S (S (K S) (\x. S (K x) (Rv F T))) (S (S (K Rv) (K T)) (K F))
-- => S (S (K S) (S (\x. S (K x)) (\x. Rv F T))) (S (S (K Rv) (K T)) (K F))
-- => S (S (K S) (S (S (K S) K) (\x. Rv F T))) (S (S (K Rv) (K T)) (K F))
-- => S (S (K S) (S B (S (\x. Rv F) (\x. T)))) (S (S (K Rv) (K T)) (K F))
-- => S (S (K S) (S B (S (S (K Rv) (K F)) (K T)))) (S (S (K Rv) (K T)) (K F))


-- xor' :: SKI (Bool' (Bool' a -> Bool' a) -> Bool' a -> Bool' a)
xor' = Ap (Ap S (Ap (Ap S (Ap K S)) (Ap (Ap S comp) (Ap (Ap S (Ap (Ap S (Ap K rotv)) (Ap K false))) (Ap K true))))) (Ap (Ap S (Ap (Ap S (Ap K rotv)) (Ap K true))) (Ap K false))
-- xor' = Ap (Ap S (Ap (Ap S (Ap K S)) (Ap (Ap S (Ap (Ap comp S) K)) (Ap (Ap S (Ap K K)) (Ap K not'))))) (Ap (Ap S (Ap K K)) (Ap K true))