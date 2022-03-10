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
                        , NatF(..)
                        , Nat
                        , RAlgebra
                        -- * Recursion schemes
                        , ana
                        , cata
                        , para
                        -- * Coalgebra's
                        , buildListF
                        , buildCoalg
                        -- * Algebra's
                        , lengthAlg
                        , lengthListF
                        , lengthListF'
                        , fromNat
                        , toNat
                        -- * Utilities
                        , toList
                        ) where

import           Data.Function ((&))

-- | Generalised fixed point for any functor /f/.
-- Note that @unFix (Fix x) == x@
newtype Fix f = Fix { unFix :: f (Fix f) }

-- This requires UndecidableInstances because the context is larger
-- than the head and so GHC can't guarantee that the instance safely
-- terminates. (Copied from Data.Functor.Fixedpoint).
instance Show (f (Fix f)) => Show (Fix f) where
  showsPrec p (Fix f) = showsPrec p f

instance Eq (f (Fix f)) => Eq (Fix f) where
  Fix x == Fix y = x == y
  Fix x /= Fix y = x /= y

-- | List Functor where r is the carrier type.
data ListF a r = NilF | ConsF a r deriving (Functor, Show, Eq)

-- | Natural numbers Functor.
data NatF r = ZeroF | SuccF r deriving (Show, Functor)

-- | Natural numbers type.
type Nat = Fix NatF

-- | R-Algebra
type RAlgebra f a = Fix f -> f a -> a

-- | Anamorphism - produce a structure.
ana :: Functor f => (a -> f a) -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg

-- | Catamorphism - consume a structure.
cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

-- | Paramorphism - improved consumption of a structure.
para :: Functor f => RAlgebra f a -> Fix f -> a
para ralg t = unFix t & fmap (para ralg) & ralg t

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

-- | Length using special case of paramorphism.
lengthListF' :: Fix (ListF a) -> Int
lengthListF' = para (const lengthAlg)

-- | Convert Natural number to an integer.
fromNat :: Nat -> Int
fromNat = cata alg where
  alg ZeroF     = 0
  alg (SuccF n) = n + 1

-- | Build a natural number from an interger.
toNat :: Int -> Nat
toNat = ana coalg where
  coalg n
    | n <= 0    = ZeroF
    | otherwise = SuccF (n - 1)

-- | Convert a `ListF` to a standard list.
toList :: Fix (ListF a) -> [a]
toList = cata alg
  where alg ls = case ls of
                  NilF      -> []
                  ConsF a r -> a : r
