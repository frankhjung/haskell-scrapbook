{- |

Module      : MyType
Description : Type applications
Copyright   : © Frank Jung, 2022
License     : GPL-3

-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module MyType (typeName) where

import           Data.Data  (Typeable, typeRep)
import           Data.Proxy (Proxy (..))

-- | Proxy to record type. Proxy is equivalent to the unit type, @()@. But
-- also has a phantom type parameter @a@, which is used to keep track of
-- the type.
-- (Use type from `Data.Proxy`)
-- data Proxy a = Proxy

-- | Show ambigous types.
-- Hindley-Milner type system can only infer types to the right of the
-- context arrow (@=>@), which means the type parameter @a@ can never be
-- correctly inferred. Haskell refers to such a type as /ambiguous/.
--
-- >>> typeName @Int
-- "Int"
--
-- >>> typeName @String
-- "[Char]"
--
-- >>> typeName @(Maybe [Int])
-- "Maybe [Int]"
typeName :: forall a. Typeable a => String
typeName = show . typeRep $ Proxy @a
