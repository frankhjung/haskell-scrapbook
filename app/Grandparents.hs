{-

== LogicT

Example from <https://github.com/Bodigrim/logict#readme Haskell logict Package>.

See also <https://medium.com/twelve-days-of-monad/day-8-logict-a-list-transformer-a03dabba79cf LogicT — A List Transformer>

-}

import           Control.Applicative   (empty, (<|>))
import           Control.Monad         (guard)
import           Control.Monad.Logic   (Logic (..), LogicT (..), observeAll)
import           Data.Functor.Identity (Identity)

-- >>> do { x <- pure 0 <|> pure 1 <|> pure 2; if even x then pure x else empty } :: [Int]
-- [0,2]
-- >>> [x | x <- [0..2], even x]
-- [0,2]

-- | The known parent and child.
parents :: [ (String, String) ]
parents = [ ("Sarah",  "John")
          , ("Arnold", "John")
          , ("John",   "Anne")
          ]

-- | Find all grandparents for given name.
grandparent :: String -> Logic String
grandparent grandchild = do (p, c) <- choose parents
                            (c', g) <- choose parents
                            guard (c == c')
                            guard (g == grandchild)
                            pure p

-- | List of possible parents
choose :: [a] -> LogicT Identity a
choose = foldr ((<|>) . pure) empty

-- | Run example: find all grandparents of 'Anne'.
main :: IO ()
main = do
  let grandparents = observeAll (grandparent "Anne")
  putStrLn $ "Anne's grandparents are: " <> show grandparents
