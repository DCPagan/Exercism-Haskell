module Pangram (isPangram) where

import Data.Char
import Data.List

isPangram :: String -> Bool
isPangram = isSubsequenceOf (enumFromTo 'A' 'Z')
  . sort . nub . map toUpper . filter isLetter
