{-# LANGUAGE OverloadedStrings #-}
{-

== Knights & Knaves

The following puzzle appears in
<https://www.doverpublications.com/mathsci/ms13w51/497054.pdf The Gödelian
Puzzle Book: Puzzles, Paradoxes and Proofs> by Raymond Smullyan.

Let's make another trip to the Island of Knights and Knaves. Recall that
knights always tell the truth and knaves always lie. We meet three natives
and we are reliably informed that exactly one of the three is a magician.

They make these statements:

(1) A: "B is not both a knave and a magician."
(2) B: "Either A is a knave or I am not a magician."
(3) C: "The magician is a knave."

Only **one** of the natives is a magician.

What conclusions can we draw from these statements?

=== Solution

>>> cabal exec magicians
A: (Knight, Magician), B:(Knight, Muggle), C:(Knave, Muggle)

=== Explanation

Known information:

(1) Knights tell the truth.
(2) Knaves always lie.
(3) There is only one magician.
(4) Claim made by A: "B is not both a knave and a magician."
(5) Claim made by B: "Either A is a knave or I am not a magician."
(6) Claim made by C: "The magician is a knave."

From C’s statement it follows that C cannot be the magician, because if he
is a knight then the magician is really a knave and hence cannot be C. On
the other hand if C is a knave, then contrary to his statement, the
magician is not a knave but a knight, hence cannot be C who is a knave.
Thus in either case, C is not the magician.

Next we will see that A must be a knight. Well, suppose he were a knave.
Then his statement is false, which means that B must be both a knave and a
magician. Since A is a knave (under our assumption) then it is true that
either A is a knave or (anything else!). Thus it is true that either A is a
knave or B is not the magician, but this is just what B said, and thus the
knave B made a true statement which is not possible! Thus the assumption
that A is a knave leads to an impossibility, hence A cannot be a knave.
Thus A is a knight. Hence his statement is true, which means that B is not
both a knave and a magician.

We now know the following:

(1) C is not the magician.
(2) A is a knight.
(3) B is not both a knave and a magician.

Next we will see that B cannot be the magician, for suppose he were. Then
it is false that he is not the magician, and it is false that A is a knave
(by (2)), hence both alternatives of B’s statement are false. Hence B’s
statement must be false, which makes B a knave. Hence B is then both a
knave and a magician, which is contrary to (3)! Thus it cannot be that B is
the magician. Also C is not the magician, as we have seen. Thus it must be
A who is the magician. Also, since B is not the magician, what he said is
true, hence B is a knight. As for C, what he said cannot be true, since the
magician is really a knight (A), not a knave. Hence C is a knave.

In summary, A and B are both knights, C is a knave and the magician is A.

-}

module Main(main) where

import           Control.Monad (guard)
import           Fmt           (fmt, (+|), (|+))

-- | Knaves always lie.
-- Knights always tell the truth.
data Role = Knave | Knight deriving (Eq, Show, Ord, Enum, Bounded)

-- | Abilitites of a native: they are a Muggle or Magician.
data Ability = Muggle | Magician deriving (Eq, Show, Ord, Enum, Bounded)

-- | Natives of the island have a role and are possible a magician.
data Native = Native {
                       _r :: Role,
                       _m :: Ability
                     } deriving (Eq)

instance Show Native where
  show (Native r m) = fmt "("+|show r|+", "+|show m|+")"

-- | The possible solution for the native roles.
data Solution = Solution {
                           _a :: Native,
                           _b :: Native,
                           _c :: Native
                         } deriving (Eq)

instance Show Solution where
  show (Solution a b c) = fmt "A: "+|show a|+", B:"+|show b|+", C:"+|show c|+""

-- | Verify A's claim that "B is not both a knave and a magician."
-- This uses B's statement which precludes A being a Knave as
-- this would lead to a contradiction here.
isClaimAValid :: Native -> Native -> Native -> Bool
isClaimAValid (Native Knight _) (Native Knave  Muggle) _ = True
isClaimAValid (Native Knight _) (Native Knight _)      _ = True
isClaimAValid _ _ _                                      = False

-- | Verify B's claim that "Either A is a knave or I am not a magician."
isClaimBValid :: Native -> Native -> Native -> Bool
isClaimBValid (Native Knight _) (Native Knight Muggle)  _ = True
isClaimBValid _ _ _                                       = False

-- | Verify C's claim that "The magician is a knave."
-- C cannot be a magician as that would be a contradiction.
isClaimCValid :: Native -> Native -> Native -> Bool
isClaimCValid (Native Knave Magician) (Native _     Muggle)   (Native Knight Muggle) = True
isClaimCValid (Native _     Muggle)   (Native Knave Magician) (Native Knight Muggle) = True
isClaimCValid _                        _                      (Native Knave  Muggle) = True
isClaimCValid _ _ _                                                                  = False

-- | Count the number of Magicians in this group of Natives.
countMagicians :: Native -> Native -> Native -> Int
countMagicians (Native _ m1) (Native _ m2) (Native _ m3) = sum (map fromEnum [m1, m2, m3])

-- | Helper function to generate all possible Natives.
allNatives :: [Native]
allNatives = [Native r a | r <- [minBound .. maxBound], a <- [minBound .. maxBound]]

-- | Solve this puzzle.
solve :: [Solution]
solve = do
  -- use list monad to generate all possible native subjects
  nativeA <- allNatives
  nativeB <- allNatives
  nativeC <- allNatives
  -- can only have one magician
  guard $ countMagicians nativeA nativeB nativeC == 1
  -- verify claims
  guard $ isClaimAValid nativeA nativeB nativeC
  guard $ isClaimBValid nativeA nativeB nativeC
  guard $ isClaimCValid nativeA nativeB nativeC
  return Solution { _a = nativeA, _b = nativeB, _c = nativeC }

-- | Solve this Knights & Knaves problem.
main :: IO ()
main = mapM_ print solve
