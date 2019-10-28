#!/usr/bin/runhaskell

-- | Word Count as per wc(1).
-- wc -w wordcount.hs
-- 38 wordcount.hs
--
-- cat wordcount.hs | runhaskell wordcount.hs
-- 38
--
main :: IO ()
main = interact $ show . length . words
