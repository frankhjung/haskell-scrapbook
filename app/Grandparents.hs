{-
== LogicT

Example from <https://github.com/Bodigrim/logict#readme Haskell logict Package>.
-}

import           Control.Applicative   (empty, (<|>))
import           Control.Monad         (guard)
import           Control.Monad.Logic   (Logic (..), LogicT (..), observeAll)
import           Data.Functor.Identity (Identity)

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
