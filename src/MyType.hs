{- |

Module      : MyType
Description : Type applications
Copyright   : © Frank Jung, 2022-2025
License     : GPL-3

= Key Concepts Used

== Type Applications

This feature allows you to explicitly specify types in your code using the @
syntax. For example, typeName @Int explicitly applies the type Int to the
function typeName.

== Ambiguous Types

Ambiguous types occur when the type system cannot infer a type parameter because
it is not used in a way that provides enough information. For example, in
typeName, the type parameter a is only used in the phantom type of Proxy, which
doesn't provide enough information for inference.

== Scoped Type Variables

The ScopedTypeVariables extension allows type variables declared in a forall to
be used explicitly within the function body.

== Typeable

The Typeable typeclass provides runtime type information. The typeRep function
retrieves a representation of a type that can be converted to a string.

-}
{-# LANGUAGE AllowAmbiguousTypes #-}
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
-- [Proxy @a]: Creates a Proxy value with the phantom type @a@. A Proxy is a
--     placeholder that carries type information but has no runtime value.
--
-- [typeRep $ Proxy @a]: Retrieves the runtime representation of the type @a@
--     using @typeRep@.
--
-- [show]: Converts the type representation to a string.
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
