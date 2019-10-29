#!/usr/bin/runhaskell

import           System.Environment (getArgs)

-- | Alternative `main` where value read from STDIN.
-- echo n | runhaskell mod35.hs
--
-- main :: IO ()
-- main = interact $ show . mod35 . read . head . words

mod35 :: Int -> Bool
mod35 n = (n `mod` 3 == 0) || (n `mod` 5 == 0)

-- | Tests whether value modulus 3 or modulus 5?
-- To run, call:
-- runhaskell mod35.hs n
main :: IO ()
main = getArgs >>= \args -> print $ (mod35 . read . head) args

