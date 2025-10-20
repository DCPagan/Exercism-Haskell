{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Connect (Mark(..), winner) where

import Control.Applicative (Alternative(..), Applicative(..))
import Control.Arrow (Arrow(..))
import Control.Monad

import Data.Array.IArray
import Data.Array.MArray
import Data.Function (on)
import Data.List (groupBy, intersperse, uncons)

import Text.ParserCombinators.ReadP hiding (many)
import Text.ParserCombinators.ReadPrec (lift)
import Text.Read (Read(..))

data Mark
  = Cross
  | Nought
  | Empty
  deriving (Eq, Enum, Bounded)

type Hex = Array (Int, Int) Mark

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

instance {-# OVERLAPPING #-}Read Hex where
  readPrec = lift hex
  readListPrec = many readPrec

instance Show Mark where
  show = return . fromMark
  showList = (++) . intersperse ' ' . fmap fromMark

instance {-# OVERLAPPING #-}Show Hex where
  show =
    unlines . zipWith ((++) . flip replicate ' ') (enumFrom 0) . fmap show
    . toAssocRows
  showList = (++) . unlines . fmap show

winner :: [String] -> Maybe Mark
winner board = undefined
