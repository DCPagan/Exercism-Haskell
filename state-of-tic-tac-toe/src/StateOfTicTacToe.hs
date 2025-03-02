{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
module StateOfTicTacToe (gameState, GameState(..)) where

import Control.Arrow
import Control.Applicative.Combinators
import Data.Array.IArray
import Data.Bool
import Data.Function
import Data.Maybe
import Data.List
import Data.List.Extra (unsnoc)
import Text.ParserCombinators.ReadP (ReadP, char, get, pfail)
import Text.ParserCombinators.ReadPrec (lift)
import Text.Read (readPrec, readListPrec)

data GameState
  = WinX
  | WinO
  | Draw
  | Ongoing
  | Impossible
  deriving (Bounded, Enum, Eq, Show)

instance Semigroup GameState where
  Ongoing <> x = x
  x <> Ongoing = x
  Draw <> x = x
  x <> Draw = x
  Impossible <> _ = Impossible
  _ <> Impossible = Impossible
  x <> y
    | x == y = x
    | otherwise = Impossible

instance Monoid GameState where
  mempty = Ongoing

data Cell
  = Empty
  | X
  | O
  deriving (Bounded, Enum, Eq, Ord)

type Coord = (Word, Word)
type Assoc = (Coord, Cell)
type Board = Array Coord Cell

toCell :: Char -> Maybe Cell
toCell = \case
  ' ' -> Just Empty
  'X' -> Just X
  'O' -> Just O
  _ -> Nothing

fromCell :: Cell -> Char
fromCell = \case
  Empty -> ' '
  X -> 'X'
  O -> 'O'

readCell :: ReadP Cell
readCell = toCell <$> get >>= maybe pfail return

readColAssocs :: ReadP [(Word, Cell)]
readColAssocs = zip (enumFrom 0) <$> many readCell

readAssocRows :: ReadP [[Assoc]]
readAssocRows = zipWith (fmap . first . (,)) (enumFrom 0)
  <$> endBy readColAssocs (char '\n')

readAssocs :: ReadP [Assoc]
readAssocs = concat <$> readAssocRows

assocsToBoard :: [Assoc] -> Board
assocsToBoard xs = array (lower, upper) xs
  where
    lower = maybe (0, 1) (fst . fst) $ uncons xs
    upper = maybe (0, 0) (fst . snd) $ unsnoc xs

inputToBoard :: [String] -> Maybe Board
inputToBoard = 
  fmap (assocsToBoard . concat . zipWith (fmap . first . (,)) (enumFrom 0))
  . traverse (fmap (zip (enumFrom 0)) . traverse toCell)

instance Read Cell where
  readPrec = lift readCell
  readListPrec = lift $ many readCell

instance Show Cell where
  show = return . fromCell
  showList = (++) . fmap fromCell

instance {-# OVERLAPPING #-} Read Board where
  readPrec = lift $ assocsToBoard <$> readAssocs

instance {-# OVERLAPPING #-} Show Board where
  show = unlines . fmap (fmap $ fromCell . snd)
    . groupBy (on (==) $ fst . fst) . assocs

isOTurn :: Board -> Maybe Bool
isOTurn board = case turnCount board of
  0 -> Just False
  1 -> Just True
  _ -> Nothing

turnCount :: Board -> Word
turnCount = uncurry (-)
  . (genericLength . filter (X ==) &&& genericLength . filter (O ==))
  . fmap snd . assocs

validTurnCount :: Board -> GameState
validTurnCount = bool Impossible Ongoing . inRange (0, 1) . turnCount

fullBoard :: Board -> Bool
fullBoard = all (Empty /=) . fmap snd . assocs

isDraw :: Board -> GameState
isDraw board = if fullBoard board
  then if fromMaybe False $ isOTurn board
    then Draw
    else Impossible
  else Ongoing

verticalRange :: Board -> [Word]
verticalRange = range . (fst *** fst) . bounds

horizontalRange :: Board -> [Word]
horizontalRange = range . (snd *** snd) . bounds

lineMatch :: [Cell] -> GameState
lineMatch = foldr f Impossible
  where
    f Empty _ = Ongoing
    f X Impossible = WinX
    f O Impossible = WinO
    f X WinX = WinX
    f O WinO = WinO
    f _ _ = Ongoing

horizontalWin :: Board -> GameState
horizontalWin board = mconcat $ do
  y <- verticalRange board
  return $ lineMatch [board ! (y, x) | x <- horizontalRange board]

verticalWin :: Board -> GameState
verticalWin board = mconcat $ do
  x <- horizontalRange board
  return $ lineMatch [board ! (y, x) | y <- verticalRange board]

diagonalWin :: Board -> GameState
diagonalWin board
  = lineMatch . fmap ((board !) . (id &&& id)) $ verticalRange board

diagonalWin' :: Board -> GameState
diagonalWin' board
  = lineMatch . fmap (board !) . (zip <*> reverse) $ verticalRange board

boardToState :: Board -> GameState
boardToState = validTurnCount
  <> isDraw
  <> horizontalWin
  <> verticalWin
  <> diagonalWin
  <> diagonalWin'

gameState :: [String] -> GameState
gameState = maybe Impossible boardToState . inputToBoard
