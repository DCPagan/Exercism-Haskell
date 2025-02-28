{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
module Minesweeper where

import Control.Applicative
import Control.Applicative.Combinators
import Control.Arrow
import Control.Lens
import Control.Lens.Extras
import Data.Array.IArray
import Data.Char
import Data.Coerce
import Data.Maybe
import Data.List hiding (uncons)
import Text.ParserCombinators.ReadP (ReadP, char, eof, get, pfail)
import Text.ParserCombinators.ReadPrec (lift)
import Text.Read (readPrec, readListPrec)

type Coord = (Word, Word)

data Tile where
  Mine :: Tile
  Adjacents :: Word -> Tile
  deriving Eq
makePrisms ''Tile

newtype Minefield where
  Minefield :: {
    getMinefield :: Array Coord Tile
  } -> Minefield
  deriving Eq

toTile :: Char -> Maybe Tile
toTile '*' = Just Mine
toTile ' ' = Just $ Adjacents 0
toTile x | isDigit x = Just $ Adjacents $ fromIntegral $ digitToInt x
toTile _ = Nothing

fromTile :: Tile -> Char
fromTile Mine = '*'
fromTile (Adjacents 0) = ' '
fromTile (Adjacents x) = intToDigit $ fromIntegral x

eol :: ReadP ()
eol = () <$ char '\n' <|> eof

predBound :: (Enum a, Ord a) => a -> a -> a
predBound a b = if a < b then pred b else b

succBound :: (Enum a, Ord a) => a -> a -> a
succBound a b = if a > b then succ b else b

getLower :: Integral a => a -> a -> a
getLower x y = if y <= x then succ x else 0

readTile :: ReadP Tile
readTile = toTile <$> get >>= maybe pfail return

readTileColAssocs :: ReadP [(Word, Tile)]
readTileColAssocs = zip (enumFrom 0) <$> many readTile

readTileColAssocRows :: ReadP [[(Word, Tile)]]
readTileColAssocRows = endBy readTileColAssocs (char '\n') <* eof

readTileAssocRows :: ReadP [[(Coord, Tile)]]
readTileAssocRows
  = zipWith (fmap . first . (,)) (enumFrom 0) <$> readTileColAssocRows

readTileAssocs :: ReadP [(Coord, Tile)]
readTileAssocs = concat <$> readTileAssocRows

assocRowsToMinefield :: [[(Coord, Tile)]] -> Minefield
assocRowsToMinefield rows
  = let
    as = concat rows
    len = genericLength rows
    wid = fromMaybe 0 . fmap (genericLength . fst) . uncons $ rows
    lower = (getLower 0 len, getLower 0 wid)
    upper = (predBound 0 len, predBound 0 wid)
  in
    Minefield $ array (lower, upper) as

instance Read Tile where
  readPrec = lift readTile
  readListPrec = many readPrec

instance Show Tile where
  show = singleton . fromTile
  showList = (++) . fmap fromTile

instance Read Minefield where
  readPrec = lift $ assocRowsToMinefield <$> readTileAssocRows

instance Show Minefield where
  show (Minefield minefield) = unlines $ do
    y <- enumFromTo yLower yUpper
    return [fromTile $ minefield ! (y, x) | x <- enumFromTo xLower xUpper]
    where
      ((yLower, xLower), (yUpper, xUpper)) = bounds minefield

countAdjacentMines :: Array Coord Tile -> Coord -> Tile -> Tile
countAdjacentMines _ _ Mine = Mine
countAdjacentMines minefield coord (Adjacents _)
  = Adjacents . genericLength . filter (is _Mine) . fmap (minefield !)
    . range . (adjacentLower &&& adjacentUpper) $ coord
    where
      ((yLower, xLower), (yUpper, xUpper)) = bounds minefield
      adjacentLower :: Coord -> Coord
      adjacentLower = predBound yLower *** predBound xLower
      adjacentUpper :: Coord -> Coord
      adjacentUpper = succBound yUpper *** succBound xUpper

annotateMinefield :: Minefield -> Minefield
annotateMinefield = coerce $ countAdjacentMines >>= imap

readMinefield :: [String] -> Minefield
readMinefield = read . unlines

showMinefield :: Minefield -> [String]
showMinefield = lines . show

annotate :: [String] -> [String]
annotate = showMinefield . annotateMinefield . readMinefield
