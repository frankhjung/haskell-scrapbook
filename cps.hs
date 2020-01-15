{-
Example from https://en.wikibooks.org/wiki/Haskell/Continuation_passing_style

Uses the Cont monad from the transformers package.

Using callCC is better than using return as argument is a function and it
returns a suspended computation.

echo 3 4 | runhaskell ./cps.hs
25


-}

import           Control.Monad.Trans.Cont (Cont, callCC, runCont)

addCont :: Int -> Int -> Cont r Int
addCont x y = callCC $ \k -> k (x + y)

squareCont :: Int -> Cont r Int
squareCont x = callCC $ \k -> k (x * x)

pythagorasCont :: Int -> Int -> Cont r Int
pythagorasCont x y = do
    x_squared <- squareCont x
    y_squared <- squareCont y
    addCont x_squared y_squared

solve :: [Int] -> String
solve (a:b:_) = runCont (pythagorasCont a b) show
solve []      = "Nothing"
solve _       = "Nothing"

main :: IO ()
main = interact $ solve . map read . words
