{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
module WordSearch where

import Control.Applicative
import Control.Applicative.Combinators
import Control.Arrow
import Control.Lens
import Control.Monad
import Data.Array.IArray
import Data.Function
import Data.List hiding (uncons)
import qualified Data.List.NonEmpty as L
import Data.Maybe
import Data.Monoid
import Text.Read (lift, readListPrec, readPrec)
import Text.ParserCombinators.ReadP (ReadP, char, get)
import Safe

data CharPos where
  CharPos :: {
    row :: Int,
    col :: Int
  } -> CharPos
  deriving (Eq, Ord, Show)

data WordPos where
  WordPos :: {
    start :: CharPos,
    end :: CharPos
  } -> WordPos
  deriving (Eq, Show)

data Direction
  = North
  | NorthEast
  | East
  | SouthEast
  | South
  | SouthWest
  | West
  | NorthWest
  deriving (Bounded, Enum, Eq, Ord, Show)

type Coord = (Word, Word)
type Assoc = ((Word, Word), Char)
type Grid = Array Coord Char

enumerate :: (Bounded a, Enum a) => [a]
enumerate = enumFromTo minBound maxBound

readColAssocs :: ReadP [(Word, Char)]
readColAssocs = zip enumerate <$> many get

readAssocs :: ReadP [Assoc]
readAssocs = concat . zipWith (fmap . first . (,)) enumerate
  <$> endBy readColAssocs (char '\n')

getLowerBound :: [Assoc] -> Coord
getLowerBound = maybe (0, 1) (fst . fst) . uncons

getUpperBound :: [Assoc] -> Coord
getUpperBound = maybe (0, 0) (fst . snd) . unsnoc

assocsToGrid :: [Assoc] -> Grid
assocsToGrid = (getLowerBound &&& getUpperBound) >>= array

inputToGrid :: [String] -> Grid
inputToGrid = assocsToGrid . concat . zipWith (fmap . first . (,)) enumerate
  . fmap (zip enumerate)

instance {-# OVERLAPPING #-} Read Grid where
  readPrec = lift $ assocsToGrid <$> readAssocs
  readListPrec = sepBy readPrec (lift $ char '\n')

instance {-# OVERLAPPING #-} Show Grid where
  show = unlines . fmap (fmap snd) . groupBy (on (==) $ fst . fst) . assocs
  showList = (++) . intercalate "\n" . fmap show

toCharPos :: Coord -> CharPos
toCharPos = uncurry $ on CharPos (fromIntegral . succ)

toWordPos :: (Coord, Coord) -> WordPos
toWordPos = uncurry WordPos . (each %~ toCharPos)

nextCoord :: Direction -> Coord -> Maybe Coord
nextCoord = \case
  North -> _1 predMay
  NorthEast -> _1 predMay >=> _2 succMay
  East -> _2 succMay
  SouthEast -> _1 succMay >=> _2 succMay
  South -> _1 succMay
  SouthWest -> _1 succMay >=> _2 predMay
  West ->_2 predMay 
  NorthWest -> _1 predMay >=> _2 predMay

inBounds :: Grid -> Coord -> Maybe Coord
inBounds grid = (<$) <*> guard . inRange (bounds grid)

coordRay :: Grid -> Direction -> Coord -> [Coord]
coordRay grid dir
  = unfoldr (fmap (id &&& (nextCoord dir >=> inBounds grid))) . inBounds grid

coordRays :: Grid -> Coord -> [[Coord]]
coordRays = flip (traverse . coordRay) enumerate

headAndTail :: [a] -> Maybe (a, a)
headAndTail = (_1 headMay >=> _2 lastMay) . (id &&& id)

readCoordRay :: Grid -> [Coord] -> String
readCoordRay grid = catMaybes . fmap ((grid ^?) . ix)

readCoordRaysAndPos :: Grid -> Coord -> [(String, [Coord])]
readCoordRaysAndPos grid
  = fmap (readCoordRay grid &&& id) . coordRays grid

searchRay :: String -> (String, [Coord]) -> (String, Maybe (Coord, Coord))
searchRay word (ray, coords)
  = (word,
    guard (isPrefixOf word ray) *> headAndTail (take (length word) coords))

orderSearchResults :: [String] -> [(String, Maybe WordPos)]
  -> [(String, Maybe WordPos)]
orderSearchResults wordList results = snd $ foldr f (results, []) wordList
  where
    f word (remnant, ordered) = (
      filter ((word /=) . fst) remnant, 
      (fromMaybe (word, Nothing) $ find ((word ==) . fst) remnant) : ordered)

search :: [String] -> [String] -> [(String, Maybe WordPos)]
search gridRows wordList
  = let
    grid = inputToGrid gridRows
  in
    orderSearchResults wordList
      . fmap (fst . L.head &&& getAlt . foldMap (Alt . snd))
      . L.groupBy (on (==) fst) . sortOn fst
      $ do
        coord <- range (bounds grid)
        ray <- readCoordRaysAndPos grid coord
        word <- wordList
        return $ _2 . _Just %~ toWordPos $ searchRay word ray
