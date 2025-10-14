{-# LANGUAGE MultiWayIf #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}

module IsbnVerifier (isbn) where

import Control.Monad

import Data.Char (digitToInt, isDigit)
import Data.Foldable (find)
import Data.Maybe

import Text.ParserCombinators.ReadP
  ( ReadP
  , char
  , get
  , readP_to_S
  , satisfy
  , skipMany)

decimal :: ReadP Int
decimal = do
  skipMany (char '-')
  digitToInt <$> satisfy isDigit

finalDigit :: ReadP Int
finalDigit = do
  skipMany (char '-')
  x <- get
  if
    | isDigit x -> return $ digitToInt x
    | x == 'X' -> return 10
    | otherwise -> fail "Invalid digit"

parseIsbn :: ReadP [Int]
parseIsbn = do
  decimals <- replicateM 9 decimal
  final <- finalDigit
  return $ decimals ++ [final]

evalIsbn :: [Int] -> Bool
evalIsbn = (0 ==) . flip mod 11 . sum . zipWith (*) (reverse $ enumFromTo 1 10)

readMaybe :: ReadP a -> String -> Maybe a
readMaybe r = fmap fst . find (null . snd) . readP_to_S r

isbn :: String -> Bool
isbn = maybe False evalIsbn . readMaybe parseIsbn
