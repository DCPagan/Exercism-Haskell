module Anagram where

import Data.Char
import Data.Function
import Data.Functor.Contravariant
import Data.List

isPermutation :: String -> String -> Bool
isPermutation = on (==) (sort . fmap toUpper)

notSame :: String -> String -> Bool
notSame = on (/=) $ fmap toUpper

isAnagramFor :: String -> String -> Bool
isAnagramFor = getEquivalence $ Equivalence isPermutation <> Equivalence notSame

anagramsFor :: String -> [String] -> [String]
anagramsFor = filter . isAnagramFor
