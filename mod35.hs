import           System.Environment (getArgs)

mod35 :: Int -> Bool
mod35 n = (n `mod` 3 == 0) || (n `mod` 5 == 0)

-- Version 1
-- runhaskell mod35.hs n
-- main :: IO ()
-- main = do
--   args <- getArgs
--   let n = (read . head) args
--   print (mod35 n)

-- Version 2
-- runhaskell mod35.hs n
main :: IO ()
main = getArgs >>= \args -> print $ (mod35 . read . head) args

-- Version 3
-- echo n | runhaskell mod35.hs
-- main :: IO ()
-- main = interact $ show . mod35 . read . head . words

