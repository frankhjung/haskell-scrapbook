module Main (main) where

-- | Count words from file read from STDIN.
--
-- Word Count as per <https://linux.die.net/man/1/wc wc(1)>.
--
-- >>> wc -w wordcount.hs
-- 38 wordcount.hs
--
-- >>> cat wordcount.hs | runhaskell wordcount.hs
-- 38
main :: IO ()
main = interact $ show . length . words
