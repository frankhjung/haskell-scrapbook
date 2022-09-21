{-|

Module      : Caesar
Description : Caesar cipher, ROT13 and ROT135
Copyright   : © Frank Jung, 2022
License     : GPL-3

Code from <https://www.manning.com/books/haskell-bookcamp Haskell Bookcamp>
by Philipp Hagenlocher

-}

module Caesar ( caesar
              , rot13
              , rot135
              -- Supporting functions
              , indexOf
              , isDigit
              , isLower
              , isMisc
              , isUpper
              , alphabetRot
              , digitRot
              , lowerRot
              , upperRot
              , rotChar
              , rotCharDigit
              -- Data structures
              , Alphabet
              , digits
              , lowerAlphabet
              , upperAlphabet
              ) where

type Alphabet = String

lowerAlphabet :: Alphabet
lowerAlphabet = ['a'..'z']

upperAlphabet :: Alphabet
upperAlphabet = ['A'..'Z']

digits :: Alphabet
digits = ['0'..'9']

isLower :: Char -> Bool
isLower = flip elem lowerAlphabet

isUpper :: Char -> Bool
isUpper = flip elem upperAlphabet

isDigit :: Char -> Bool
isDigit = flip elem digits

isMisc :: Char -> Bool
isMisc = flip notElem (lowerAlphabet ++ upperAlphabet ++ digits)

indexOf :: Char -> String -> Int
indexOf _ []     = error "Search string cannot be empty"
indexOf c (x:xs) = if c == x then 0 else 1 + indexOf c xs

alphabetRot :: Alphabet -> Int -> Char -> Char
alphabetRot as n c = as !! ((indexOf c as + n) `mod` length as)

lowerRot :: Int -> Char -> Char
lowerRot = alphabetRot lowerAlphabet

upperRot :: Int -> Char -> Char
upperRot = alphabetRot upperAlphabet

digitRot :: Int -> Char -> Char
digitRot = alphabetRot digits

rotChar :: Int -> Char -> Char
rotChar n c
  | isLower c = lowerRot n c
  | isUpper c = upperRot n c
  | otherwise = c

rotCharDigit :: Int -> Int -> Char -> Char
rotCharDigit n m c
  | isDigit c = digitRot m c
  | otherwise = rotChar n c

caesar :: Int -> String -> String
caesar n = map (rotChar n)

rot13 :: String -> String
rot13 = caesar 13

rot135 :: String -> String
rot135 = map (rotCharDigit 13 5)
