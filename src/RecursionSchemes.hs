{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

{-|

Module      : RecursionSchemes
Description : Examples of Ana/Cata/Para-morphisms recursion schemes
Copyright   : © Frank Jung, 2021, 2022
License     : GPL-3

A collection of recursion scheme examples.

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

== Paramorphism

The paramorphism examples come from "Making Sense of Recursion Patterns".
A paramorphism is like a catamorphism, but it preserves the initial data
structure.

= References

* <https://stackoverflow.com/questions/48023348/deriving-a-functor-for-an-infinite-stream Deriving a functor for an infinite stream>
* <https://dl.acm.org/doi/abs/10.5555/2663689.2663693 Making Sense of Recursion Patterns by Paul Bailes and Leighton Brough>

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
                        , para'
                        , para''
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
                        , insert
                        , insert'
                        , toList
                        , foldXs
                        , idx
                        , idx'
                        , idx''
                        , idx'''
                        ) where

import           Data.Bool     (bool)
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

-- | Paramorphism where input list is the first parameter.
-- This comes from
-- [Making Sense of Recursion Patterns](https://dl.acm.org/doi/abs/10.5555/2663689.2663693)
-- by Paul Bailes and Leighton Brough. It extends `foldr` by supplying to the
-- combining operation (op) the unprocessed list tail, in addition to the head
-- and the result of recursion on the tail as provided by `foldr`.
--
-- Sum a list:
--
-- >>> para' (const . (+)) 0 [1,2,3]
-- 6
--
-- Suffixes of a list:
--
-- >>> para' (const (:)) [] "abcd"
-- ["bcd","cd","d",""]
--
para' :: (a -> [a] -> b -> b) -> b -> [a] -> b
para' _ b []      = b
para' op b (x:xs) = op x xs (para' op b xs)

-- | Paramorphism using `foldr`.
-- This comes from
-- [Making Sense of Recursion Patterns](https://dl.acm.org/doi/abs/10.5555/2663689.2663693)
-- by Paul Bailes and Leighton Brough.
--
-- The following shows how to get a catamorphism from a paramorphism.
-- In this example, we are calculating the sum of items from a list.
--
-- Sum a list:
--
-- >>> para'' (const . (+)) 0 [1,2,3]
-- 6
--
-- Suffixes of a list:
--
-- >>> para'' (\ _ xs xss -> xs : xss) [] "abcd"
-- ["bcd","cd","d",""]
--
-- >>> para'' (const (:)) [] "abcd"
-- ["bcd","cd","d",""]
--
para'' :: (a -> [a] -> b -> b) -> b -> [a] -> b
para'' op b xs = snd $ foldr go ([], b) xs
  where
    go y (ys, ys') = (y:ys, op y ys ys')

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

-- | Insert element into list at correct ordered position using `foldr`.
--
-- >>> insert 1 [2,3,4]
-- [1,2,3,4]
--
-- >>> insert 'c' "abde"
-- "abcde"
--
-- >>> insert 'f' "abcde"
-- "abcdef"
--
-- >>> insert 'o' "oa"
-- "ooa"
--
insert :: Ord a => a -> [a] -> [a]
insert e = snd . foldr go ([], [e])
  where
    go y (ys, yse) = (y:ys, bool (y:yse) (e:y:ys) (e <= y))

-- | Insert element into list at correct ordered position.
--
-- >>> insert' 1 [2,3,4]
-- [1,2,3,4]
--
-- >>> insert' 1 []
-- [1]
--
-- >>> insert' 'c' "abde"
-- "abcde"
--
-- >>> insert' 'c' "abde" == "abcde"
-- True
--
-- >>> insert' 'o' "oa"
-- "ooa"
--
insert' :: Ord a => a -> [a] -> [a]
insert' e = para' go [e]
  where
    go y ys yse = bool (y:yse) (e:y:ys) (e <= y)

-- | Convert a `ListF` to a standard list.
toList :: Fix (ListF a) -> [a]
toList = cata alg
  where alg ls = case ls of
                  NilF      -> []
                  ConsF a r -> a : r

-- | `foldXs` is `foldr` with the structure moved as the first parameter.
foldXs :: Foldable t => t a -> (a -> b -> b) -> b -> b
foldXs xs op b = foldr op b xs

-- | List indexing.
--
-- >>> idx "abcde"
-- [0,1,2,3,4]
idx :: (Foldable t, Enum b, Num b) => t a -> [b]
idx xs = foldXs xs (\ _ f m -> m : f (succ m)) (const []) 0

-- | Alternate list indexing.
--
-- >>> idx' "abcde"
-- [0,1,2,3,4]
idx' :: [b] -> [Integer]
idx' = zipWith const [0..]

-- | List indexing using `foldr`.
--
-- >>> idx'' "abcde"
-- [0,1,2,3,4]
idx'' :: [b] -> [Integer]
idx'' xs = foldr (\ _ f (y:ys) -> y : f ys) (const []) xs [0..]

-- | List indexing using `foldr` with parameter last.
--
-- >>> idx''' "abcde"
-- [0,1,2,3,4]
idx''' :: [b] -> [Integer]
idx''' = flip (foldr (\ _ f (y:ys) -> y : f ys) (const [])) [0..]
