{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Counting (Color(..), territories, territoryFor) where

import Control.Applicative ((<|>), liftA2, many, some)
import Control.Arrow ((&&&), first)
import Control.Lens hiding (index, uncons)
import Control.Monad
import Control.Monad.ST (ST, runST)
import Control.Monad.State

import Data.Array.IArray ((!), Array, IArray, array, assocs, bounds)
import Data.Array.MArray
  ( MArray
  , freeze
  , getAssocs
  , getBounds
  , readArray
  , thaw
  , writeArray)
import Data.Array.ST (STArray)
import Data.Function (on)
import qualified Data.IntMap.Lazy as M
import Data.IntMap.Lazy (IntMap)
import Data.Ix (range)
import Data.List (genericLength, groupBy, transpose, uncons)
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as S
import Data.Set (Set, empty)
import Data.Void (Void, vacuous)

import qualified Text.ParserCombinators.ReadP as R
import Text.ParserCombinators.ReadP (ReadP, char, eof, pfail)
import qualified Text.ParserCombinators.ReadPrec as RP
import Text.Read (readListPrec, readMaybe, readPrec)

data Color
  = Black
  | White
  deriving (Eq, Ord)

type Coord = (Int, Int)

type Tile a = (Maybe Color, a)

type GoBoard = Array Coord (Tile Int)

type RegionMap = IntMap (Set Coord, Maybe Color)

_single :: (Cons s s a a, AsEmpty s) => Traversal' s a
_single = _Cons . aside _Empty . _1

toColor :: Char -> Maybe Color
toColor 'B' = Just Black
toColor 'W' = Just White
toColor _ = Nothing

fromColor :: Color -> Char
fromColor Black = 'B'
fromColor White = 'W'

toTile :: Char -> Maybe (Tile Int)
toTile 'B' = Just (Just Black, 0)
toTile 'W' = Just (Just White, 0)
toTile ' ' = Just (Nothing, 0)
toTile _ = Nothing

fromTile :: Tile a -> Char
fromTile (Just Black, _) = 'B'
fromTile (Just White, _) = 'W'
fromTile (Nothing, _) = ' '

indexRows :: [[(Int, a)]] -> [[(Coord, a)]]
indexRows = zipWith (fmap . first . flip (,)) (enumFrom 1)

eol :: ReadP ()
eol = void (char '\n') <|> eof

readTile :: ReadP (Tile Int)
readTile = R.get >>= maybe pfail return . toTile

readTileColAssocs :: ReadP [(Int, Tile Int)]
readTileColAssocs = zip (enumFrom 1) <$> some readTile <* eol

readTileColAssocRows :: ReadP [[(Int, Tile Int)]]
readTileColAssocRows = some readTileColAssocs <* eof

readTileAssocRows :: ReadP [[(Coord, Tile Int)]]
readTileAssocRows = indexRows <$> readTileColAssocRows

assocRowsToGoBoard :: [[(Coord, Tile Int)]] -> GoBoard
assocRowsToGoBoard rows =
  let
    as = concat rows
    len = genericLength rows
    wid = maybe 0 (length . fst) $ uncons rows
    lower = (1, 1)
    upper = (wid, len)
  in
    array (lower, upper) as

instance Read Color where
  readPrec = RP.get >>= maybe RP.pfail return . toColor
  readListPrec = many readPrec

instance Show Color where
  show = return . fromColor
  showList = (++) . fmap fromColor

instance {-# OVERLAPPING #-}Show GoBoard where
  show =
    unlines . transpose . fmap (fmap (fromTile . snd)) . groupBy
      (on (==) (fst . fst)) . assocs

instance {-# OVERLAPPING #-}Read GoBoard where
  readPrec = RP.lift $ assocRowsToGoBoard <$> readTileAssocRows

voidBoard :: IArray a Void => a Coord Void
voidBoard = array ((1, 1), (0, 0)) []

vacuousBoard :: GoBoard
vacuousBoard = vacuous voidBoard

inputToGoBoard :: [String] -> GoBoard
inputToGoBoard = fromMaybe vacuousBoard . readMaybe . unlines

{--
 - Get the neighbor of a particular direction
 -}
west :: (Coord, Coord) -> Coord -> Maybe Coord
west ((westBound, _), _) (x, y) =
  if x <= westBound then Nothing else Just (pred x, y)

north :: (Coord, Coord) -> Coord -> Maybe Coord
north ((_, northBound), _) (x, y) =
  if y <= northBound then Nothing else Just (x, pred y)

east :: (Coord, Coord) -> Coord -> Maybe Coord
east (_, (eastBound, _)) (x, y) =
  if x >= eastBound then Nothing else Just (succ x, y)

south :: (Coord, Coord) -> Coord -> Maybe Coord
south (_, (_, southBound)) (x, y) =
  if y >= southBound then Nothing else Just (x, succ y)

{--
 - Assocs adjacent to the coordinates.  For mutable arrays.
 -}
readAdjacents :: (Monad m, MArray a (Tile e) m, Ord e)
  => a Coord (Tile e)
  -> Coord
  -> m [(Coord, Tile e)]
readAdjacents board coord = do
  bs <- getBounds board
  let west' = west bs coord
      north' = north bs coord
      east' = east bs coord
      south' = south bs coord
  w' <- traverse (readArray board) west'
  n' <- traverse (readArray board) north'
  e' <- traverse (readArray board) east'
  s' <- traverse (readArray board) south'
  let w = liftA2 (,) west' w'
      n = liftA2 (,) north' n'
      e = liftA2 (,) east' e'
      s = liftA2 (,) south' s'
  return $ catMaybes [w, n, e, s]

{--
 - Assocs adjacent to the coordinates.  For immutable arrays.
 -}
adjacents :: (IArray a (Tile e), Ord e)
  => a Coord (Tile e)
  -> Coord
  -> [(Coord, Tile e)]
adjacents board coord =
  let
    bs = bounds board
    west' = west bs coord
    north' = north bs coord
    east' = east bs coord
    south' = south bs coord
    w' = (board !) <$> west'
    n' = (board !) <$> north'
    e' = (board !) <$> east'
    s' = (board !) <$> south'
    w = liftA2 (,) west' w'
    n = liftA2 (,) north' n'
    e = liftA2 (,) east' e'
    s = liftA2 (,) south' s'
  in
    catMaybes [w, n, e, s]

{--
 - Flood
 -}
flood :: (Monad m, MArray a (Tile Int) m)
  => a Coord (Tile Int)
  -> Int
  -> Coord
  -> m (Set Coord)
flood board serial coord = do
  (color, region) <- readArray board coord
  if region == 0
    then (do
            writeArray board coord (color, serial)
            adj <- toListOf (traverse . filteredBy (_2 . _1 . only color) . _1)
              <$> readAdjacents board coord
            mconcat . (S.singleton coord :)
              <$> traverse (flood board serial) adj)
    else return empty

{--
 - Scan for all regions.
 -}
scanTerritories
  :: (Monad m, MArray a (Tile Int) m) => a Coord (Tile Int) -> m RegionMap
scanTerritories board' = do
  _assoc <- getAssocs board'
  ixs <- range <$> getBounds board'
  mconcat <$> evalStateT (traverse (scan' board') ixs) 1
  where
    scan' :: (MonadTrans s, MonadState Int (s m), MArray a (Tile Int) m)
      => a Coord (Tile Int)
      -> Coord
      -> s m RegionMap
    scan' board coord = do
      serial <- get
      region <- lift $ flood board serial coord
      if null region then return M.empty else do
        modify succ
        (color, _regionId) <- lift $ readArray board coord
        return $ M.singleton serial (region, color)

{--
 - Mark territories and chains on a given board
 -}
scanBoard :: GoBoard -> (GoBoard, RegionMap)
scanBoard board = runST $ do
  go <- thaw board :: forall s. ST s (STArray s Coord (Tile Int))
  regions <- scanTerritories go
  (, regions) <$> freeze go

{--
 - Coordinates adjacent to the region of distinct color of the region.
 -}
regionAdjacents :: GoBoard -> Maybe Color -> Set Coord -> Set (Coord, Tile Int)
regionAdjacents board color = foldr f empty
  where
    f coord adj =
      (<> adj) . S.fromList . toListOf
        (traverse . filteredBy (_2 . _1 . nearly color (/= color))) $
      adjacents board coord

{--
 - Color which has captured this region, if any.
 -}
captor :: GoBoard -> Maybe Color -> Set Coord -> Maybe Color
captor board color =
  join . preview _single . S.toList . S.map (fst . snd)
  . regionAdjacents board color

territories :: [String] -> [(Set Coord, Maybe Color)]
territories input =
  regions ^.. traverse . filteredBy (_2 . only Nothing) & fmap
    (fst &&& uncurry (flip $ captor board))
  where
    (board, regions) = scanBoard $ inputToGoBoard input

territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor input coord =
  terr ^? traverse . filteredBy (_1 . filtered (S.member coord))
  where
    terr = territories input
