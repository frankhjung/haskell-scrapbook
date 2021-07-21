{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

{-|

Module      : RecursionSchemes
Description : Ana- and Cata- morphism recursion example using Types
Copyright   : © Frank Jung, 2021
License     : GPL-3

Source
<https://stackoverflow.com/questions/48023348/deriving-a-functor-for-an-infinite-stream Deriving a functor for an infinite stream>

= Examples

== Anamorphism

Using a non-recursive coalgebra to create a `ListF`.

@
ls = buildListF 4
show ls
"ConsF 4 (ConsF 3 (ConsF 2 (ConsF 1 NilF)))"

λ> :t unFix ls
unFix ls :: ListF Int (Fix (ListF Int))

λ> :t ls
ls :: Fix (ListF Int)
@

== Catamorphism

Using a non-recursive algebra to measure length of `ListF` entry, use a
catamorphism over the alegra to measure length of the entire `ListF`.

@
ls :: Int a => Fix (ListF a)
ls = Fix (ConsF 4 (Fix (ConsF 3 (Fix (ConsF 2 (Fix (ConsF 1 (Fix NilF))))))))

lengthListF ls
4
@

-}

module RecursionSchemes (
                        -- * Data constructors
                          Fix(..)
                        , ListF(..)
                        -- * Recursion schemes
                        , ana
                        , cata
                        -- * Coalgebra's
                        , buildListF
                        , buildCoalg
                        -- * Algebra's
                        , lengthAlg
                        , lengthListF
                        -- * Utilities
                        , toList
                        ) where

-- | List Functor where r is the carrier type.
data ListF a r = NilF | ConsF a r deriving (Functor, Show, Eq)

-- | Generalised fixed point for any functor `f`.
-- Note that @unFix (Fix x) == x@
newtype Fix f = Fix { unFix :: f (Fix f) }

-- This requires UndecidableInstances because the context is larger
-- than the head and so GHC can't guarantee that the instance safely
-- terminates. (Copied from Data.Functor.Fixedpoint).
instance (Show (f (Fix f))) => Show (Fix f)
  where showsPrec p (Fix f) = showsPrec p f

-- | Anamorphism - produce a list.
ana :: Functor f => (a -> f a) -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg

-- Catamorphism - consume a list.
cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

-- | Coalgebra is a non-recursive function to generate a `ListF` entry.
buildCoalg :: Int -> ListF Int Int
buildCoalg n
    | n <  1    = NilF
    | otherwise = ConsF n (pred n)

-- | Feed coalgebra to anamorphism.
buildListF :: Int -> Fix (ListF Int)
buildListF = ana buildCoalg

-- | A alegbra over `ListF` to get list length.
lengthAlg :: ListF a Int -> Int
lengthAlg ls = case ls of
               NilF      -> 0
               ConsF _ x -> x + 1

-- | Length is a folding operation, i.e. a Catamorphism.
lengthListF :: Fix (ListF a) -> Int
lengthListF = cata lengthAlg

-- | Convert `ListF` to a standard list type.
toList :: Fix (ListF a) -> [a]
toList = cata alg
  where alg ls = case ls of
                 NilF      -> []
                 ConsF a r -> a : r
