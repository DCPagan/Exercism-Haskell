{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
module GameOfLife where

import Control.Applicative
import Control.Applicative.Combinators
import Control.Arrow
import Control.Lens
import Data.Array.IArray
import Data.Char
import Data.Coerce
import Data.List hiding (uncons)
import Data.Maybe
import Text.Read (lift, readListPrec, readPrec)
import Text.ParserCombinators.ReadP (ReadP, char, get, pfail)

data Cell where
  Dead :: Cell
  Live :: Cell
  deriving (Bounded, Enum, Eq, Ord)

type Coord = (Word, Word)
type Assoc = (Coord, Cell)

isLive :: Cell -> Bool
isLive = toEnum . fromEnum

toCell :: Int -> Maybe Cell
toCell = \case
  0 -> Just Dead
  1 -> Just Live
  _ -> Nothing

fromCell :: Cell -> Int
fromCell = \case
  Dead -> 0
  Live -> 1

predBound :: (Enum a, Ord a) => a -> a -> a
predBound a b = if a < b then pred b else a

succBound :: (Enum a, Ord a) => a -> a -> a
succBound a b = if a > b then succ b else a

getLower :: Integral a => a -> a -> a
getLower x y = if y <= x then succ x else 0

indexCols :: [a] -> [(Word, a)]
indexCols = zip (enumFrom 0)

indexRows :: [[(Word, a)]] -> [[((Word, Word), a)]]
indexRows = zipWith (fmap . first . (,)) (enumFrom 0)

readCell :: ReadP Cell
readCell = toCell . digitToInt <$> get >>= maybe pfail return

readAssocCols :: ReadP [(Word, Cell)]
readAssocCols = indexCols <$> many readCell

readAssocRows :: ReadP [[Assoc]]
readAssocRows = indexRows <$> endBy readAssocCols (char '\n')

readAssocs :: ReadP [Assoc]
readAssocs = concat <$> readAssocRows

assocRowsToBoard :: [[Assoc]] -> Board
assocRowsToBoard rows
  = let
    as = concat rows
    len = genericLength rows
    wid = fromMaybe 0 . fmap (genericLength . fst) . uncons $ rows
    lower = (getLower 0 len, getLower 0 wid)
    upper = (predBound 0 len, predBound 0 wid)
  in
    Board $ array (lower, upper) as

inputToBoard :: [[Int]] -> Maybe Board
inputToBoard
  = fmap (assocRowsToBoard . indexRows . fmap indexCols)
    . traverse (traverse toCell)

boardToOutput :: Board -> [[Int]]
boardToOutput (Board board) = do
  y <- enumFromTo yLower yUpper
  return [fromCell $ board ! (y, x) | x <- enumFromTo xLower xUpper]
  where
    ((yLower, xLower), (yUpper, xUpper)) = bounds board

instance Read Cell where
  readPrec = lift readCell
  readListPrec = many readPrec

instance Show Cell where
  show = singleton . intToDigit . fromCell
  showList = (++) . fmap (intToDigit . fromCell)

newtype Board where
  Board :: { getBoard :: Array Coord Cell } -> Board
  deriving (Eq)

instance Read Board where
  readPrec = lift $ assocRowsToBoard <$> readAssocRows

instance Show Board where
  show = unlines . fmap (fmap intToDigit) . boardToOutput

countAdjacentLives :: Board -> Coord -> Word
countAdjacentLives (Board board) = genericLength . filter isLive . fmap (board !)
  . (delete <*> range . (adjacentLower &&& adjacentUpper))
  where
    ((yLower, xLower), (yUpper, xUpper)) = bounds board
    adjacentLower :: Coord -> Coord
    adjacentLower = predBound yLower *** predBound xLower
    adjacentUpper :: Coord -> Coord
    adjacentUpper = succBound yUpper *** succBound xUpper

tickCell :: Board -> Coord -> Cell -> Cell
tickCell board coord Live
  | let c = countAdjacentLives board coord,
    c >= 2 && c <= 3 = Live
tickCell board coord Dead
  | let c = countAdjacentLives board coord,
    c == 3 = Live
tickCell _ _ _ = Dead

tickBoard :: Board -> Board
tickBoard = coerce $ tickCell . Board >>= imap

tick :: [[Int]] -> [[Int]]
tick = maybe [] (boardToOutput . tickBoard) . inputToBoard
