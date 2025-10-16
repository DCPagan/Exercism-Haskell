{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

{-# OPTIONS_GHC -Wno-unused-record-wildcards #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Matrix
  ( Matrix(..)
  , cols
  , column
  , flatten
  , fromList
  , fromString
  , reshape
  , row
  , rows
  , shape
  , transpose) where

import Control.Arrow (Arrow(..))
import Control.Monad

import Data.List hiding (transpose)
import qualified Data.Vector as V
import Data.Vector (Vector)

import Debug.Trace

import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec (lift, readPrec_to_P)
import Text.Read (Read(..))
import Text.Read.Lex

data Matrix a where
  Matrix :: { _len :: Int, _wid :: Int, _vec :: Vector a } -> Matrix a
  deriving (Eq, Functor)

readRowP :: (Read a) => ReadP [a]
readRowP = sepBy (readPrec_to_P readPrec 0) (char ' ')

matrix :: (Read a) => ReadP (Matrix a)
matrix = do
  rs <- sepBy readRowP (char '\n') <* eof
  let widths = group $ fmap length rs
  -- |Check that all rows have the same width.
  case uncons widths of
    Just (_wid:_, []) -> do
      let _len = if _wid == 0 then 0 else length rs
          _vec = V.fromList $ join rs
      return Matrix { .. }
    _ -> pfail

instance (Read a) => Read (Matrix a) where
  readPrec = lift matrix

instance (Show a) => Show (Matrix a) where
  show Matrix { .. } = unlines $ do
    y <- enumFromTo 0 $ pred _len
    return $ unwords $ do
      x <- enumFromTo 0 $ pred _wid
      return $ show $ _vec V.! (_wid * y + x)

cols :: Matrix a -> Int
cols = _wid

column :: Int -> Matrix a -> Vector a
column x Matrix { .. } = V.generate _len ((_vec V.!) . (pred x +) . (_wid *))

flatten :: Matrix a -> Vector a
flatten = _vec

fromList :: [[a]] -> Matrix a
fromList xss =
  let
    _len = length xss
    widths = group $ fmap length xss
    (_wid, _vec) = case uncons widths of
      Just (_wid:_, []) -> (_wid, V.fromList $ join xss)
      _ -> (0, V.empty)
  in
    Matrix { .. }

fromString :: Read a => String -> Matrix a
fromString = read

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (_len, _wid) m = m { _len, _wid }

row :: Int -> Matrix a -> Vector a
row x Matrix { .. } = V.slice (pred x * _wid) _wid _vec

rows :: Matrix a -> Int
rows = _len

shape :: Matrix a -> (Int, Int)
shape = _len &&& _wid

transpose :: Matrix a -> Matrix a
transpose Matrix { .. } =
  Matrix { _len = _wid, _wid = _len, _vec = V.generate (V.length _vec) f }
  where
    f i = _vec V.! (_wid * y + x)
      where
        x = div i _len

        y = mod i _len
