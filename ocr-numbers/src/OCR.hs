{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
module OCR where

import Control.Arrow
import Control.Lens
import Control.Monad
import Data.Array.IArray
import Data.Function
import Data.List
import Data.Maybe
import Safe

data Cell
  = S
  | H
  | V
  deriving (Bounded, Enum, Eq, Ord)

instance Show Cell where
  show = return . fromCell
  showList = (++) . fmap fromCell

type Coord = (Word, Word)
type Assoc = (Coord, Cell)
type Grid = Array Coord Cell

instance {-# OVERLAPPING #-} Show Grid where
  show = unlines . fmap (fmap (fromCell . snd))
    . groupBy (on (==) (fst . fst)) . assocs

toCell :: Char -> Maybe Cell
toCell = \case
  ' ' -> Just S
  '_' -> Just H
  '|' -> Just V
  _ -> Nothing

fromCell :: Cell -> Char
fromCell = \case
  S -> ' '
  H -> '_'
  V -> '|'

getLower :: Integral a => a -> a -> a
getLower x y = if y <= x then succ x else 0

predBound :: (Enum a, Ord a) => a -> a -> a
predBound a b = if a < b then pred b else a

succBound :: (Enum a, Ord a) => a -> a -> a
succBound a b = if a > b then succ b else a

indexCols :: [a] -> [(Word, a)]
indexCols = zip (enumFrom 0)

indexRows :: [[(Word, a)]] -> [[(Coord, a)]]
indexRows = zipWith (fmap . first . (,)) (enumFrom 0)

rowsToGrid :: [[Assoc]] -> Grid
rowsToGrid rows =
  let
    ass = concat rows
    len = genericLength rows
    wid = maybe 0 genericLength $ listToMaybe rows
    lower = each %~ getLower 0 $ (len, wid)
    upper = each %~ predBound 0 $ (len, wid)
  in
    array (lower, upper) ass

toGrid :: String -> Maybe Grid
toGrid = fmap (rowsToGrid . indexRows . fmap indexCols)
  . traverse (traverse toCell) . lines

areBoundsCorrectSize :: Grid -> Bool
areBoundsCorrectSize a =
  let ((north, west), (south, east)) = bounds a
  in mod (south - north + 1) 4 == 0 && mod (east - west + 1) 3 == 0

toOcrDimensions :: Grid -> (Word, Word)
toOcrDimensions a =
  let ((north, west), (south, east)) = bounds a
  in (div (south - north + 1) 4, div (east - west + 1) 3)

toOcrCells :: Grid -> Maybe [[Grid]]
toOcrCells a
  | areBoundsCorrectSize a =
    let
      (len, wid) = toOcrDimensions a
    in
      Just $ do
        y <- enumFromTo 0 $ predSafe len
        return $ do
          x <- enumFromTo 0 $ predSafe wid
          return $ ixmap
            ((0, 0), (3, 2))
            ((\i -> 4 * y + i) *** (\j -> 3 * x + j)) a
  | otherwise = Nothing

emptyGrid :: Grid
emptyGrid = array ((1, 1), (0, 0)) []

toOcrDigit :: [String] -> Grid
toOcrDigit = fromMaybe emptyGrid . toGrid . unlines

ocrDigits :: [Grid]
ocrDigits =
  [ toOcrDigit
    [ " _ "
    , "| |"
    , "|_|"
    , "   "
    ]
  , toOcrDigit
    [ "   "
    , "  |"
    , "  |"
    , "   "
    ]
  , toOcrDigit
    [ " _ "
    , " _|"
    , "|_ "
    , "   "
    ]
  , toOcrDigit
    [ " _ "
    , " _|"
    , " _|"
    , "   "
    ]
  , toOcrDigit
    [ "   "
    , "|_|"
    , "  |"
    , "   "
    ]
  , toOcrDigit
    [ " _ "
    , "|_ "
    , " _|"
    , "   "
    ]
  , toOcrDigit
    [ " _ "
    , "|_ "
    , "|_|"
    , "   "
    ]
  , toOcrDigit
    [ " _ "
    , "  |"
    , "  |"
    , "   "
    ]
  , toOcrDigit
    [ " _ "
    , "|_|"
    , "|_|"
    , "   "
    ]
  , toOcrDigit
    [ " _ "
    , "|_|"
    , " _|"
    , "   "
    ]
  ]

ocrCellToDigit :: Grid -> Maybe Word
ocrCellToDigit = fmap fromIntegral . flip elemIndex ocrDigits

convert :: String -> String
convert = fromMaybe "-1"
  <<< fmap (intercalate "," . fmap (fmap ocrCellToDigit >=> maybe "?" show))
  <<< toOcrCells <=< toGrid 
