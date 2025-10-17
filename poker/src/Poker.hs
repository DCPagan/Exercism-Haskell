{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Poker (bestHands) where

import Control.Applicative (Alternative(..), Applicative(..))
import Control.Arrow ((>>>))
import Control.Monad ((>=>))

import Data.Char (digitToInt, isDigit)
import Data.Function (on)
import Data.List (group, groupBy, sort, sortBy, uncons)

import Text.ParserCombinators.ReadP hiding (many)
import Text.ParserCombinators.ReadPrec (lift)
import Text.Read (Read(..), readMaybe)

data Rank
  = LowAce
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace
  deriving (Eq, Ord, Enum, Bounded)

data Suit
  = Club
  | Spade
  | Heart
  | Diamond
  deriving (Eq, Ord, Enum, Bounded)

data Card where
  Card :: { _rank :: Rank, _suit :: Suit } -> Card
  deriving (Eq)

type Hand = [Card]

numericRank :: ReadP Rank
numericRank = do
  x <- digitToInt <$> satisfy isDigit
  case x of
    1 -> Ten <$ char '0' <|> return LowAce
    2 -> return Two
    3 -> return Three
    4 -> return Four
    5 -> return Five
    6 -> return Six
    7 -> return Seven
    8 -> return Eight
    9 -> return Nine
    _ -> pfail

face :: ReadP Rank
face = do
  get >>= \case
    'J' -> return Jack
    'Q' -> return Queen
    'K' -> return King
    'A' -> return Ace
    _ -> pfail

suit :: ReadP Suit
suit = do
  get >>= \case
    'C' -> return Club
    'S' -> return Spade
    'H' -> return Heart
    'D' -> return Diamond
    _ -> pfail

rank :: ReadP Rank
rank = numericRank <|> face

card :: ReadP Card
card = do
  _rank <- rank
  _suit <- suit
  return Card { .. }

hand :: ReadP [Card]
hand = sepBy card skipSpaces

instance Read Suit where
  readPrec = lift suit
  readListPrec = many readPrec

instance Read Rank where
  readPrec = lift rank
  readListPrec = many readPrec

instance Read Card where
  readPrec = lift card
  readListPrec = lift hand

instance Show Suit where
  show = \case
    Club -> "C"
    Spade -> "S"
    Heart -> "H"
    Diamond -> "D"

instance Show Rank where
  show = \case
    LowAce -> "1"
    Two -> "2"
    Three -> "3"
    Four -> "4"
    Five -> "5"
    Six -> "6"
    Seven -> "7"
    Eight -> "8"
    Nine -> "9"
    Ten -> "10"
    Jack -> "J"
    Queen -> "Q"
    King -> "K"
    Ace -> "A"

instance Show Card where
  show Card { .. } = show _rank ++ show _suit
  showList = (++) . unwords . fmap show

lowAce :: Rank -> Rank
lowAce Ace = LowAce
lowAce x = x

same :: (Eq a) => [a] -> Bool
same [] = True
same [_] = True
same (a:b:rest) = a == b && same (b:rest)

groupByRank :: [Card] -> [[Card]]
groupByRank =
  sortBy (on (flip compare) length) . groupBy (on (==) _rank) . sortBy
    (on (flip compare) _rank)

straight :: Hand -> Bool
straight h =
  ranks == take (length h) (enumFrom $ head ranks) || lowRanks
  == take (length h) (enumFrom LowAce)
  where
    ranks = sort $ _rank <$> h

    lowRanks = sort $ lowAce . _rank <$> h

flush :: Hand -> Bool
flush = same . fmap _suit

straightFlush :: Hand -> Bool
straightFlush = liftA2 (&&) straight flush

royalFlush :: Hand -> Bool
royalFlush h = flush h && sort (_rank <$> h) == enumFrom Ten

fourOfAKind :: Hand -> Bool
fourOfAKind = maybe False ((4 ==) . fst) . uncons . fmap length . groupByRank

fullHouse :: Hand -> Bool
fullHouse = ([3, 2] ==) . fmap length . groupByRank

threeOfAKind :: Hand -> Bool
threeOfAKind = maybe False ((3 ==) . fst) . uncons . fmap length . groupByRank

twoPair :: Hand -> Bool
twoPair = ([2, 2, 1] ==) . fmap length . groupByRank

onePair :: Hand -> Bool
onePair = maybe False ((2 ==) . fst) . uncons . fmap length . groupByRank

instance Ord Card where
  compare = on compare _rank

groupComparison :: Hand -> Hand -> Ordering
groupComparison = on compare (fmap _rank . concat . groupByRank)

straightComparison :: Hand -> Hand -> Ordering
straightComparison = on compare (sortBy (flip compare) . fmap (lowAce . _rank))

highCardComparison :: Hand -> Hand -> Ordering
highCardComparison = on compare (sortBy (flip compare) . fmap _rank)

instance {-# OVERLAPPING #-}Ord Hand where
  compare a b
    | royalFlush a && royalFlush b = EQ
    | royalFlush a = GT
    | royalFlush b = LT
    | straightFlush a && straightFlush b = straightComparison a b
    | straightFlush a = GT
    | straightFlush b = LT
    | fourOfAKind a && fourOfAKind b = groupComparison a b
    | fourOfAKind a = GT
    | fourOfAKind b = LT
    | fullHouse a && fullHouse b = groupComparison a b
    | fullHouse a = GT
    | fullHouse b = LT
    | flush a && flush b = highCardComparison a b
    | flush a = GT
    | flush b = LT
    | straight a && straight b = straightComparison a b
    | straight a = GT
    | straight b = LT
    | threeOfAKind a && threeOfAKind b = groupComparison a b
    | threeOfAKind a = GT
    | threeOfAKind b = LT
    | twoPair a && twoPair b = groupComparison a b
    | twoPair a = GT
    | twoPair b = LT
    | onePair a && onePair b = groupComparison a b
    | onePair a = GT
    | onePair b = LT
    | otherwise = highCardComparison a b

instance {-# OVERLAPPING #-}Eq Hand where
  a == b = case compare a b of
    EQ -> True
    _ -> False

bestHands :: [String] -> Maybe [String]
bestHands =
  traverse (readMaybe @Hand) >=> uncons . group . sortBy (flip compare) >>> fmap
    (fmap show . fst)
