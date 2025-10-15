{-# OPTIONS_GHC -Wno-type-defaults #-}

module Affine (decode, encode) where

import Control.Arrow (Arrow(..))

import Data.Char

inverseKey :: (Int, Int) -> Maybe (Int, Int)
inverseKey (a, b) =
  if gcd a 26 /= 1
    then Nothing
    else Just (mod (a ^ 11) 26, mod (-b) 26)

encodeLetter :: Char -> Int
encodeLetter = subtract (ord 'a') . ord . toLower

decodeLetter :: Int -> Char
decodeLetter = chr . (ord 'a' +) . flip mod 26

encipherLetter :: (Int, Int) -> Char -> Char
encipherLetter (a, b) = decodeLetter . (b +) . (a *) . encodeLetter

decipherLetter :: (Int, Int) -> Char -> Char
decipherLetter (a, b) = decodeLetter . (a *) . (b +) . encodeLetter

encipherChar :: (Int, Int) -> Char -> Char
encipherChar (a, b) c =
  if isLetter c
    then encipherLetter (a, b) $ toLower c
    else c

decipherChar :: (Int, Int) -> Char -> Char
decipherChar (a, b) c =
  if isLetter c
    then decipherLetter (a, b) $ toLower c
    else c

fives :: String -> String
fives "" = ""
fives s = uncurry (++) $ second f $ splitAt 5 s
  where
    f t =
      if null t
        then t
        else ' ':fives t

decode :: (Int, Int) -> String -> Maybe String
decode key cipherText =
  flip fmap (filter isAlphaNum cipherText) . decipherChar <$> inverseKey key

encode :: (Int, Int) -> String -> Maybe String
encode key@(a, _) plainText =
  if gcd a 26 /= 1
    then Nothing
    else Just $ fives $ encipherChar key <$> filter isAlphaNum plainText
