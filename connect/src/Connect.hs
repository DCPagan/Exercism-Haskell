{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Connect (Mark(..), winner) where

import Control.Applicative (Alternative(..), Applicative(..))
import Control.Monad

import Data.List (intersperse)

import Text.ParserCombinators.ReadP hiding (many)
import Text.ParserCombinators.ReadPrec (lift)
import Text.Read (Read(..))

data Mark
  = Cross
  | Nought
  | Empty
  deriving (Eq, Enum, Bounded)

type Hex = [[Mark]]

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

parallelogram :: ReadP Hex
parallelogram = do
  r <- marks
  newline
  rs <- parallelogram' (length r) 1 <|> [] <$ eof
  return $ r : rs
  where
    parallelogram' :: Int -> Int -> ReadP Hex
    parallelogram' i j = do
      r <- row i j
      newline
      rs <- parallelogram' i (succ j) <|> [] <$ eof
      return $ r : rs

instance {-# OVERLAPPING #-}Read Hex where
  readPrec = lift parallelogram
  readListPrec = many readPrec

instance Show Mark where
  show = return . fromMark
  showList = (++) . intersperse ' ' . fmap fromMark

instance {-# OVERLAPPING #-}Show Hex where
  show = unlines . zipWith ((++) . flip replicate ' ') (enumFrom 0) . fmap show
  showList = (++) . unlines . fmap show

winner :: [String] -> Maybe Mark
winner board = undefined
