{-# LANGUAGE GADTs #-}
module DNA (toRNA) where

import Control.Applicative
import Data.List
import Text.ParserCombinators.ReadP as R hiding (many)
import Text.ParserCombinators.ReadPrec as RP
import Text.Read

data DNANucleotide where
  DNA_Adenosine :: DNANucleotide
  DNA_Cytosine :: DNANucleotide
  DNA_Guanine :: DNANucleotide
  DNA_Thymine :: DNANucleotide

data RNANucleotide where
  RNA_Adenosine :: RNANucleotide
  RNA_Cytosine :: RNANucleotide
  RNA_Guanine :: RNANucleotide
  RNA_Urasil :: RNANucleotide

toDNANucleotide :: Char -> Either Char DNANucleotide
toDNANucleotide 'A' = Right DNA_Adenosine
toDNANucleotide 'C' = Right DNA_Cytosine
toDNANucleotide 'G' = Right DNA_Guanine
toDNANucleotide 'T' = Right DNA_Thymine
toDNANucleotide x = Left x

fromDNANucleotide :: DNANucleotide -> Char
fromDNANucleotide DNA_Adenosine = 'A'
fromDNANucleotide DNA_Cytosine = 'C'
fromDNANucleotide DNA_Guanine = 'G'
fromDNANucleotide DNA_Thymine = 'T'

instance Read DNANucleotide where
  readPrec = lift $ toDNANucleotide <$> R.get >>= either (fail . singleton) return
  readListPrec = many readPrec

instance Show DNANucleotide where
  show = singleton . fromDNANucleotide
  showList = (++) . (>>= show)

toRNANucleotide :: Char -> Either Char RNANucleotide
toRNANucleotide 'A' = Right RNA_Adenosine
toRNANucleotide 'C' = Right RNA_Cytosine
toRNANucleotide 'G' = Right RNA_Guanine
toRNANucleotide 'U' = Right RNA_Urasil
toRNANucleotide x = Left x

fromRNANucleotide :: RNANucleotide -> Char
fromRNANucleotide RNA_Adenosine = 'A'
fromRNANucleotide RNA_Cytosine = 'C'
fromRNANucleotide RNA_Guanine = 'G'
fromRNANucleotide RNA_Urasil = 'U'

instance Read RNANucleotide where
  readPrec = lift $ toRNANucleotide <$> R.get >>= either (fail . singleton) return
  readListPrec = many readPrec

instance Show RNANucleotide where
  show = singleton . fromRNANucleotide
  showList = (++) . (>>= show)

transcribe :: DNANucleotide -> RNANucleotide
transcribe DNA_Guanine = RNA_Cytosine
transcribe DNA_Cytosine = RNA_Guanine
transcribe DNA_Thymine = RNA_Adenosine
transcribe DNA_Adenosine = RNA_Urasil

toRNA :: String -> Either Char String
toRNA = traverse $ fmap (fromRNANucleotide . transcribe) . toDNANucleotide
