{-# LANGUAGE TupleSections #-}
module DNA where

import Control.Applicative
import Control.Arrow
import Data.Function
import Data.List
import qualified Data.List.NonEmpty as L
import Data.Map (Map)
import qualified Data.Map as M
import Text.ParserCombinators.ReadPrec
import Text.Read

data Nucleotide = A | C | G | T deriving (Bounded, Enum, Eq, Ord)

toNucleotide :: Char -> Either String Nucleotide
toNucleotide 'A' = Right A
toNucleotide 'C' = Right C
toNucleotide 'G' = Right G
toNucleotide 'T' = Right T
toNucleotide _ = Left "error"

fromNucleotide :: Nucleotide -> Char
fromNucleotide A = 'A'
fromNucleotide C = 'C'
fromNucleotide G = 'G'
fromNucleotide T = 'T'

instance Read Nucleotide where
  readPrec = toNucleotide <$> get >>= either fail return
  readListPrec = many readPrec

instance Show Nucleotide where
  show = return . fromNucleotide
  showList = (++) . (>>= show)

emptyCount :: Integral i => [(Nucleotide, i)]
emptyCount = fmap (, 0) $ enumFrom minBound

mergeEmpty :: Integral i => [(Nucleotide, i)] -> [(Nucleotide, i)]
mergeEmpty = flip (unionBy (on (==) fst)) emptyCount

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts =
  fmap (M.fromList . mergeEmpty . fmap (L.head &&& L.length) . L.group . sort)
    . traverse toNucleotide
