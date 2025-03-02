{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Queens (boardString, canAttack) where

import Control.Applicative.Combinators
import Control.Arrow
import Data.Coerce
import Data.Array.IArray
import Data.Array.MArray
import Data.Array.ST
import Data.Function
import Data.List
import Data.List.Extra (unsnoc)
import Data.Monoid
import Text.ParserCombinators.ReadP (ReadP, char, get, pfail)
import Text.ParserCombinators.ReadPrec (lift)
import Text.Read (readListPrec, readPrec)

data Cell
  = Empty
  | White
  | Black
  deriving (Bounded, Enum, Eq, Ord)

type Coord = (Int, Int)
type Assoc = (Coord, Cell)
type ChessBoard = Array Coord Cell

toCell :: Char -> Maybe Cell
toCell = \case
  '_' -> Just Empty
  'W' -> Just White
  'B' -> Just Black
  _ -> Nothing

fromCell :: Cell -> Char
fromCell = \case
  Empty -> '_'
  White -> 'W'
  Black -> 'B'

readCell :: ReadP Cell
readCell = toCell <$> get >>= maybe pfail return

readColAssocs :: ReadP [(Int, Cell)]
readColAssocs = zip (enumFrom 0) <$> sepBy readCell (char ' ')

readAssocRows :: ReadP [[Assoc]]
readAssocRows = zipWith (fmap . first . (,)) (enumFrom 0)
  <$> endBy readColAssocs (char '\n')

readAssocs :: ReadP [Assoc]
readAssocs = concat <$> readAssocRows

assocsToBoard :: [Assoc] -> ChessBoard
assocsToBoard as =
  let
    lower = maybe (0, 1) (fst . fst) $ uncons as
    upper = maybe (0, 0) (fst . snd) $ unsnoc as
  in
    array (lower, upper) as

instance Read Cell where
  readPrec = lift readCell
  readListPrec = many readPrec

instance Show Cell where
  show = return . fromCell
  showList = (++) . fmap fromCell

instance {-# OVERLAPPING #-} Read ChessBoard where
  readPrec = lift $ assocsToBoard <$> readAssocs

instance {-# OVERLAPPING #-} Show ChessBoard where
  show = unlines . fmap (intersperse ' ' . fmap (fromCell . snd))
    . groupBy (on (==) $ fst . fst) . assocs

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black = show $ runSTArray $ do
  board <- newArray ((0, 0), (7, 7)) Empty
  _ <- traverse (flip (writeArray board) White) white
  _ <- traverse (flip (writeArray board) Black) black
  return board

sameRank :: Coord -> Coord -> Bool
sameRank = on (==) fst

sameFile :: Coord -> Coord -> Bool
sameFile = on (==) snd

sameDiag :: Coord -> Coord -> Bool
sameDiag = on (==) (uncurry (-))

sameDiag' :: Coord -> Coord -> Bool
sameDiag' = on (==) (uncurry (+))

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack = coerce
  $ (Any .) . sameRank
  <> (Any .) . sameFile
  <> (Any .) . sameDiag
  <> (Any .) . sameDiag'
