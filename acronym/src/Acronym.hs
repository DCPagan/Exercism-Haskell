module Acronym where

import Control.Monad
import Data.Char
import Data.Function
import Data.Functor.Contravariant
import qualified Data.List.NonEmpty as L
import Data.Monoid

isWordChar :: Char -> Bool
isWordChar = getAny . (Any . isLetter <> Any . ('\'' ==))

subwordEquivalence :: Char -> Char -> Bool
subwordEquivalence a b = isUpper a || isLower b

wordEquivalence :: Char -> Char -> Bool
wordEquivalence =
  getEquivalence $
    (Equivalence subwordEquivalence) <> (isWordChar >$< defaultEquivalence)

abbreviateCamelCase :: L.NonEmpty Char -> [Char]
abbreviateCamelCase (h L.:| t) = if any isLower t
  then return h ++ filter isUpper t
  else return h

abbreviate :: String -> String
abbreviate =
  fmap toUpper . filter isLetter
    . (L.groupBy (on (==) isWordChar) >=> abbreviateCamelCase)
