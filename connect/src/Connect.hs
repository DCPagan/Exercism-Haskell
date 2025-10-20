{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Connect (Mark(..), winner) where

import Control.Applicative (Alternative(..), Applicative(..))
import Control.Arrow (Arrow(..))
import Control.Monad
import Control.Monad.Free
import Control.Monad.ST
import Control.Monad.Trans (MonadTrans(..))

import Data.Array.IArray
import Data.Array.MArray
import Data.Array.ST
import Data.Function (on)
import Data.Functor.Identity (Identity, runIdentity)
import Data.List (groupBy, intersperse, singleton, uncons)
import Data.Maybe (fromMaybe)

import Text.ParserCombinators.ReadP hiding (many)
import qualified Text.ParserCombinators.ReadPrec as R
import Text.Read (Read(..), readMaybe)

data Mark
  = Cross
  | Nought
  | Empty
  deriving (Eq, Enum, Bounded)

type Hex = Array (Int, Int) Mark

type HexM m a = (Monad m, MArray a Mark m) => a (Int, Int) Mark

newtype ListT m a = ListT { runListT :: m [a] }

type List = ListT Identity

runList :: List a -> [a]
runList = runIdentity . runListT

instance Functor m => Functor (ListT m) where
  fmap f = ListT . fmap (fmap f) . runListT

instance Applicative m => Applicative (ListT m) where
  pure = ListT . pure . pure
  liftA2 f (ListT m) (ListT n) = ListT $ (liftA2 $ liftA2 f) m n

instance Monad m => Monad (ListT m) where
  ListT m >>= k = ListT $ m >>= fmap join . traverse (runListT . k)

instance Monad m => Alternative (ListT m) where
  empty = ListT $ pure []
  ListT m <|> ListT n = ListT do
    a <- m
    b <- n
    return $ a <|> b

instance Monad m => MonadPlus (ListT m)

instance MonadTrans ListT where
  lift = ListT . fmap singleton

instance Monad m => MonadFree [] (ListT m) where
  wrap = ListT . fmap join . traverse runListT

toMark :: Char -> Maybe Mark
toMark = \case
  'X' -> Just Cross
  'O' -> Just Nought
  '.' -> Just Empty
  _ -> Nothing

fromMark :: Mark -> Char
fromMark = \case
  Cross -> 'X'
  Nought -> 'O'
  Empty -> '.'

mark :: ReadP Mark
mark = get >>= maybe pfail return . toMark

marks :: ReadP [Mark]
marks = sepBy mark (char ' ')

row :: Int -> Int -> ReadP [Mark]
row i j = do
  m <- replicateM_ j (char ' ') >> marks
  if length m == i
    then return m
    else pfail

newline :: ReadP ()
newline = void $ char '\n'

toAssocs :: Integral i => [[a]] -> [((i, i), a)]
toAssocs =
  concat . zipWith (fmap . first . (,)) (enumFrom 0) . fmap (zip (enumFrom 0))

toAssocRows :: Ix i => Array (i, i) a -> [[a]]
toAssocRows = fmap (fmap snd) . groupBy (on (==) (fst . fst)) . assocs

hex :: ReadP Hex
hex = do
  r <- marks
  newline
  rs <- parallelogram (length r) 1 <|> [] <$ eof
  let rows = r : rs
      len = length rows
      wid = maybe 0 (length . fst) $ uncons rows
  return $ array ((0, 0), (pred len, pred wid)) $ toAssocs rows
  where
    parallelogram :: Int -> Int -> ReadP [[Mark]]
    parallelogram i j = do
      r <- row i j
      newline
      rs <- parallelogram i (succ j) <|> [] <$ eof
      return $ r : rs

voidHex :: Hex
voidHex = array ((1, 1), (0, 0)) []

readHex :: String -> Hex
readHex = fromMaybe voidHex . readMaybe

emptyHex :: Int -> Int -> Hex
emptyHex len wid = listArray ((0, 0), (pred len, pred wid)) $ repeat Empty

instance {-# OVERLAPPING #-}Read Hex where
  readPrec = R.lift hex
  readListPrec = many readPrec

instance Show Mark where
  show = singleton . fromMark
  showList = (++) . intersperse ' ' . fmap fromMark

instance {-# OVERLAPPING #-}Show Hex where
  show =
    unlines . zipWith ((++) . flip replicate ' ') (enumFrom 0) . fmap show
    . toAssocRows
  showList = (++) . unlines . fmap show

lowersForMark :: Mark -> Hex -> [(Int, Int)]
lowersForMark m h = case m of
  Cross -> let ((i, j), (_, k)) = bounds h in range ((i, j), (k, j))
  Nought -> let ((i, j), (k, _)) = bounds h in range ((i, j), (i, k))
  Empty -> []

upperForMark :: Mark -> Hex -> Int
upperForMark = \case
  Cross -> snd . snd . bounds
  Nought -> fst . snd . bounds
  Empty -> const (-1)

atMarkLimit :: Mark -> Hex -> (Int, Int) -> Bool
atMarkLimit = \case
  Cross -> on (==) snd . snd . bounds
  Nought -> on (==) fst . snd . bounds
  Empty -> const $ const True

adjacents :: (Int, Int) -> [(Int, Int)]
adjacents (i, j) =
  [ (pred i, j)
  , (pred i, succ j)
  , (i, pred j)
  , (i, succ j)
  , (succ i, pred j)
  , (succ i, j)
  ]

walk' :: MArray a Mark m
  => Hex
  -> HexM m a
  -> Mark
  -> (Int, Int)
  -> ListT m [(Int, Int)]
walk' h hm m loc = do
  let hexBounds = bounds h
  guard $ inRange hexBounds loc
  let point = h ! loc
  step <- lift $ readArray hm loc
  guard $ m == point && step == Empty
  lift $ writeArray hm loc m
  if atMarkLimit m h loc
    then return [loc]
    else wrap $ fmap (loc :) . walk' h hm m <$> adjacents loc

walk
  :: (Monad m, MArray a Mark m) => Hex -> HexM m a -> Mark -> m [[(Int, Int)]]
walk h hm m = runListT $ wrap $ walk' h hm m <$> lowersForMark m h

{-|
  "Player O" plays from top to bottom; "Player X" plays from left to right.
-}
winner :: [String] -> Maybe Mark
winner board = runST do
  let hexBoard = readHex $ unlines board
  activeHex <- newListArray (bounds hexBoard) $ repeat Empty :: forall s.
    ST s (STArray s (Int, Int) Mark)
  crossWitnesses <- walk hexBoard activeHex Cross
  noughtWitnesses <- walk hexBoard activeHex Nought
  return $ if
    | not $ null crossWitnesses -> Just Cross
    | not $ null noughtWitnesses -> Just Nought
    | otherwise -> Nothing
