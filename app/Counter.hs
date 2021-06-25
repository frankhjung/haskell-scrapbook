{-# LANGUAGE FlexibleContexts #-}

{-

Use 'fix' to recurse.

From
<https://blog.sumtypeofway.com/posts/introduction-to-recursion-schemes.html Introduction to recursion schemes>
-}

module Main (main) where

import           Data.Function      (fix)
import           Numeric.Natural    (Natural)
import           System.Environment (getArgs)

-- | A counter from 1 to a given natural number.
counter :: Natural -> [Natural]
counter n = fix (\rec xs@(x:_) -> if x > 1 then rec (x - 1:xs) else xs) [n]

-- | Count to a given natural number.
main :: IO ()
main = getArgs >>= \args -> case length args of
  1 -> let n = (read . head) args in print (counter n)
  _ -> error "Usage: [number]\nCount up to a given natural number."

