{-# LANGUAGE TupleSections #-}
module Luhn where

import Data.Char
import Data.Maybe
import Data.Monoid

allDigits :: String -> Bool
allDigits = all $ getAny . (Any . isDigit <> Any . isSpace)

stripSpaces :: String -> String
stripSpaces = filter $ not . isSpace

normalize :: String -> Maybe [Word]
normalize s
  | length (stripSpaces s) <= 1 = Nothing
  | allDigits s = Just . fmap (fromIntegral . digitToInt) . stripSpaces $ s
  | otherwise = Nothing

lSubtract :: Word -> Word
lSubtract x = if x < 9 then x else x - 9

lDouble :: Word -> Word
lDouble = lSubtract . (2 *)

lDigit :: Word -> Word -> Word
lDigit a b = if odd a then lDouble b else b

lDigits :: [Word] -> [Word]
lDigits = zipWith lDigit (enumFrom 0) . reverse

lChecksum :: [Word] -> Word
lChecksum = flip mod 10 . sum . lDigits

isValid :: String -> Bool
isValid = fromMaybe False . fmap ((0 ==) . lChecksum) . normalize
