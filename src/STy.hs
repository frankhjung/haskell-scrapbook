{-# LANGUAGE GADTs #-}

-- | Defines a singleton type for the 'Bool' and 'Int types.
-- Explore Generalized Algebraic Data Types (GADTs) from the
-- talk by Richard Eisenberg, https://youtu.be/6snteFntvjM.
module STy (STy, zero) where

data STy ty where
  SInt :: STy Int
  SBool :: STy Bool
  SMaybe :: STy a -> STy (Maybe a)

-- | The function returns either a 'SInt', 'SBool' or 'SMaybe'.
-- That is it can return either 'Int', 'Bool' or Maybe.
-- You can't normally do this in Haskell without the GADT extension.
zero :: STy a -> a
zero SInt       = 0
zero SBool      = False
zero (SMaybe _) = Nothing
