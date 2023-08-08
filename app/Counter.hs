{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

{-

Use 'fix' to recurse.

Modified from algorithm described in
<https://blog.sumtypeofway.com/posts/introduction-to-recursion-schemes.html Introduction to recursion schemes>.

Next, build a fixed-point combinator in the type system.

@
type Y t = t (t (t (t (t ...))))
@

That is a data type @Y@ that, when given another type @f@, wraps an @f@
whose children are of type @(Y f)@.

-}

module Main (main) where

import           Data.Function      (fix)
import           Data.List          (unfoldr)
import           Numeric.Natural    (Natural)
import           System.Environment (getArgs)

-- | A counter from 1 to a natural number, `n`.
-- Same as @[1..n]@
countUp :: Natural -> [Natural]
countUp n = fix (\rec xs@(x:_) -> if x > 1 then rec (x - 1:xs) else xs) [n]

-- | Count down from a natural number, `n` to 1.
-- Same as @[n,n-1..1]@
countDown :: Natural -> [Natural]
countDown = unfoldr (\n -> if n == 0 then Nothing ; else Just (n, n - 1))

-- | Count to a given natural number.
main :: IO ()
main = getArgs >>= \args -> case length args of
  1 -> let n = (read . head) args in mapM_ print [countUp n, countDown n]
  _ -> error "Usage: [number]\nCount up to a given natural number."

