{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

{-|

Module      : Expr
Description : Simple evaluator for lambda calculus expressions.
Copyright   : © Frank Jung, 2023
License     : GPL-3

Generalized Algebraic Datatypes example from
<https://link.springer.com/book/10.1007/978-3-540-76786-2 Datatype-Generic Programming>.

-}

module Expr (Expr(..), eval) where

data Expr a where
    Num :: Int -> Expr Int
    Plus :: Expr Int -> Expr Int -> Expr Int
    Eq :: Expr Int -> Expr Int -> Expr Bool
    If :: Expr Bool -> Expr e -> Expr e -> Expr e

eval :: Expr e -> e
eval (Num n)       = n
eval (Plus e1 e2)  = eval e1 + eval e2
eval (Eq e1 e2)    = eval e1 == eval e2
eval (If e1 e2 e3) = if eval e1 then eval e2 else eval e3
