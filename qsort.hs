#!/usr/bin/runhaskell

-- Programming in Haskell by Graham Hutton

-- qsort
--
-- echo -e "3 5 1 2 4 2 \n" | runhaskell qsort.hs
-- 1 2 2 3 4 5
--
-- If a <= x is replaced with a < x, then only unique values are reported.
--
-- echo -e "1 3 5 1 2 4 2 \n" | runhaskell qsort.hs
-- 1 2 3 4 5
--
-- To reverse sort, switch smaller and larger in qsort.
--

qsort :: (Show a, Ord a) => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger  = [b | b <- xs, b > x]

main :: IO ()
main = interact $ unwords . qsort . words

