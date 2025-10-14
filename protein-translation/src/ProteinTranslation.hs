{-# LANGUAGE LambdaCase #-}

module ProteinTranslation (proteins) where

import Control.Applicative
import Control.Monad

import qualified Data.Map as M

import Text.ParserCombinators.ReadP (ReadP, get, pfail)
import Text.Read (Read(..), lift, readMaybe)

data RNANucleotide
  = U
  | A
  | G
  | C
  deriving (Eq, Ord, Enum, Bounded)

type RNACodon = (RNANucleotide, RNANucleotide, RNANucleotide)

toNucleotide :: Char -> Maybe RNANucleotide
toNucleotide = \case
  'U' -> Just U
  'A' -> Just A
  'G' -> Just G
  'C' -> Just C
  _ -> Nothing

fromNucleotide :: RNANucleotide -> Char
fromNucleotide = \case
  U -> 'U'
  A -> 'A'
  G -> 'G'
  C -> 'C'

data Amino
  = Methionine
  | Phenylalanine
  | Leucine
  | Serine
  | Tyrosine
  | Cysteine
  | Tryptophan
  | Stop
  deriving (Eq, Ord, Show, Enum, Bounded)

nucleotide :: ReadP RNANucleotide
nucleotide = maybe pfail return . toNucleotide =<< get

instance Read RNANucleotide where
  readPrec = lift nucleotide
  readListPrec = many readPrec

instance Show RNANucleotide where
  show = return . fromNucleotide
  showList = (++) . fmap fromNucleotide

codonTable :: M.Map RNACodon Amino
codonTable =
  M.fromList
    [ ((A, U, G), Methionine)
    , ((U, U, U), Phenylalanine)
    , ((U, U, C), Phenylalanine)
    , ((U, U, A), Leucine)
    , ((U, U, G), Leucine)
    , ((U, C, U), Serine)
    , ((U, C, C), Serine)
    , ((U, C, A), Serine)
    , ((U, C, G), Serine)
    , ((U, A, U), Tyrosine)
    , ((U, A, C), Tyrosine)
    , ((U, G, U), Cysteine)
    , ((U, G, C), Cysteine)
    , ((U, G, G), Tryptophan)
    , ((U, A, A), Stop)
    , ((U, A, G), Stop)
    , ((U, G, A), Stop)
    ]

translate :: RNACodon -> Maybe Amino
translate = flip M.lookup codonTable

groupCodons :: [RNANucleotide] -> Maybe [RNACodon]
groupCodons [] = Just []
groupCodons (a:b:c:rest) = ((a, b, c):) <$> groupCodons rest
groupCodons _ = Nothing

proteins :: String -> Maybe [String]
proteins =
  fmap (fmap show . takeWhile (Stop /=)) . traverse translate <=< groupCodons
  <=< readMaybe
