{-# LANGUAGE OverloadedStrings #-}
{-

== Knights & Knaves Problem #26 and #28

This is just a warm up exercise for the Magicians puzzle. The structure follows
the ideas layed out in
<https://wiki.haskell.org/Logic_programming_example Haskell Wiki: Logic Programming Example>.

On an island there are two types of inhabitants:

1. Knights - who always tell the truth
2. Knaves - who always lie

=== Problem #26

Three of the islands inhabitants: A, B, and C are standing together in a garden.
A stranger passed by and asks A, "Are you a knight or a knave?" A answered, but
rather indistinctly, so the stranger could not make out what he said.

1. The stranger then asked B, "What did A say?" B replied, "A said that he is a knave."
2. At this point the third man, C, said, "Don' t believe B; he is lying!"

What are B and C?

=== Solution #26

If A is telling the truth, then he would not call himself a Knave.

If A was lying, then he would not call himself a Knave.

So B must be lying when he said that A said he was a Knave.

C claimed B was lying, which is true.

So, B is a Knave and C is a Knight.

However, we don't know what A is, so we can't say anything about him.

=== Problem #28

You come across two inhabitants of an island: A and B.

A makes the claim, "At least one of us is a knave."

What is A and B?

=== Solution #28

If both are knaves, then claim is true, so A didn't lie, which is a contradiction.
If one is a knave, then claim is true, so A must be a knight. So B is the knave.
If none are knaves, then claim is false, which is a contradiction.

So A is a Knight and B is a Knave.

=== Example

>>> cabal exec knights
A: Unknown, B: Knave, C: Knight
A: Knight, B: Knave, C: Unknown

=== Resources

1. <https://en.wikipedia.org/wiki/Knights_and_Knaves Knights and Knaves>
1. <https://cs.bme.hu/~szeredi/ait/Smullyan-What-is-the-Name-of-This-Book.pdf What is the Name of This Book?>

-}

module Main(main) where

import           Control.Monad (guard)
import           Fmt           (fmt, (+|), (|+))

-- | Knaves always lie.
-- Knights always tell the truth.
data Role = Unknown | Knave | Knight deriving (Eq, Show)

-- | The possible solution for the native roles.
data Solution = Solution {
  _a :: Role,
  _b :: Role,
  _c :: Role
} deriving (Eq)

-- | Custom show instance for the solution.
instance Show Solution where
  show (Solution a b c) = fmt $ "A: "+|show a|+", B: "+|show b|+", C: "+|show c|+""

-- | Verify B's statement that "A said that he is a knave."
--                Role of B -> Role of C -> Bool
isClaimBValid :: Role -> Role -> Bool
isClaimBValid Knave  Knight = True
isClaimBValid _     _       = False

-- | Verify C's statement that "Don't believe B; he is lying."
--                Role of B -> Role of C -> Bool
isClaimCValid :: Role -> Role -> Bool
isClaimCValid Knave Knight = True
isClaimCValid _     _      = False

-- | Solve problem #26 from Smullyan's "What is the Name of This Book?
-- This is just to show the structure for solving this type of puzzle.
solve26 :: [Solution]
solve26 = do
  -- use list monad to generate all possible solutions
  subjectB <- [Knave, Knight]
  subjectC <- [Knave, Knight]
  -- use a guard to filter out invalid solutions
  guard $ isClaimBValid subjectB subjectC
  guard $ isClaimCValid subjectB subjectC
  return Solution { _a = Unknown, _b = subjectB, _c = subjectC }

-- >>> fmap id solve26
-- [A: Unknown, B: Knave, C: Knight]
isClaimAValid :: Role -> Role -> Bool
isClaimAValid Knave  _      = False -- knave can't tell a truth
isClaimAValid Knight Knight = False -- knight can't lie
isClaimAValid Knight Knave  = True
isClaimAValid _      _      = False -- unknown can't tell a truth or lie

-- | Solve problem #28 from Smullyan's "What is the Name of This Book?
solve :: (Role -> Role -> Bool) -- Predicate function
      -> [Solution]             -- Solutions to the problem
solve p = do
  subjectA <- [Knave, Knight]
  subjectB <- [Knave, Knight]
  guard $ p subjectA subjectB
  return $ Solution { _a = subjectA, _b = subjectB, _c = Unknown }

-- >>> fmap id (solve isClaimAValid)
-- [A: Knight, B: Knave, C: Unknown]

-- | Solve the Knights & Knaves problem.
main :: IO ()
main = do
  mapM_ print solve26
  mapM_ print (solve isClaimAValid)
