{-# LANGUAGE LambdaCase #-}
module Hamming (distance) where

import Control.Applicative
import Data.Function
import Data.Monoid
import Text.ParserCombinators.ReadP (ReadP, get, pfail)
import Text.ParserCombinators.ReadPrec (lift)
import Text.Read (readPrec, readListPrec)

data Nucleotide = A | C | G | T deriving (Bounded, Enum, Eq, Ord)

toNucleotide :: Char -> Maybe Nucleotide
toNucleotide = \case
  'A' -> Just A
  'C' -> Just C
  'G' -> Just G
  'T' -> Just T
  _ -> Nothing

fromNucleotide :: Nucleotide -> Char
fromNucleotide = \case
  A -> 'A'
  C -> 'C'
  G -> 'G'
  T -> 'T'

readNucleotide :: ReadP Nucleotide
readNucleotide = toNucleotide <$> get >>= maybe pfail return

instance Read Nucleotide where
  readPrec = lift readNucleotide
  readListPrec = many readPrec

instance Show Nucleotide where
  show = return . fromNucleotide
  showList = (++) . fmap fromNucleotide

zipWithDefaultA :: Applicative f =>
  (a -> f c) -> (b -> f c) -> (a -> b -> f c) -> [a] -> [b] -> f [c]
zipWithDefaultA _ _ _ [] [] = pure []
zipWithDefaultA f _ _ xs [] = traverse f xs
zipWithDefaultA _ g _ [] ys = traverse g ys
zipWithDefaultA f g h (x:xs) (y:ys)
  = liftA2 (:) (h x y) (zipWithDefaultA f g h xs ys)

distance :: String -> String -> Maybe Int
distance = (fmap (getSum . foldMap (Sum . fromEnum)) .)
  . on (zipWithDefaultA (const Nothing) (const Nothing) (liftA2 (/=)))
    (fmap toNucleotide)
