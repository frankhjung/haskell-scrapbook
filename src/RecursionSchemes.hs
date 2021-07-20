{-# LANGUAGE DeriveFunctor #-}

{-|

Module      : RecursionSchemes
Description : Ana- and Cata- morphism recursion example using Types
Copyright   : © Frank Jung, 2021
License     : GPL-3

Source
<https://stackoverflow.com/questions/48023348/deriving-a-functor-for-an-infinite-stream Deriving a functor for an infinite stream>

= Example - Catamorphism

@
ls = In (ConsF 4 (In (ConsF 3 (In (ConsF 2 (In (ConsF 1 (In NilF))))))))

lengthListF ls
4
@

-}

module RecursionSchemes (Fix, cata, ListF, lenAlg, lengthListF, ana) where

-- | List Functor where r is the carrier type.
data ListF a r = NilF | ConsF a r deriving (Functor)

-- | Generalised fixed point for any functor `f`.
-- Note that @out (In x) == x@
newtype Fix f = In { out :: f (Fix f) }

-- Catamorphism - consume a list.
cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . out

-- | A alegbra over ListF to get list length.
lenAlg :: ListF a Int -> Int
lenAlg ls = case ls of
            NilF      -> 0
            ConsF _ x -> x + 1

-- | Length is a folding operation, i.e. a Catamorphism.
lengthListF :: Fix (ListF a) -> Int
lengthListF = cata lenAlg

-- | Anamorphism.
ana :: Functor f => (a -> f a) -> a -> Fix f
ana coalg = In . fmap (ana coalg) . coalg

