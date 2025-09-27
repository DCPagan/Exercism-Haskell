{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module RunLength (decode, encode) where

import Control.Applicative

import Data.Char (isDigit)
import Data.List (genericLength, uncons)
import Data.Monoid (Endo(..))

import qualified Text.ParserCombinators.ReadP as R
import Text.ParserCombinators.ReadP
  ( (<++)
  , ReadP
  , char
  , eof
  , get
  , munch
  , readP_to_S)
import qualified Text.ParserCombinators.ReadPrec as RP
import Text.Read hiding ((<++), get)
import Text.Read.Lex (readDecP)

data RLE where
  RLE :: { _len :: Word, _chr :: Char } -> RLE
  deriving (Eq)

readRunRLE :: ReadP RLE
readRunRLE = do
  _len <- readDecP
  _chr <- get
  return RLE { .. }

readSingleRLE :: ReadP RLE
readSingleRLE = do
  let _len = 1
  _chr <- get
  return RLE { .. }

readRLE :: ReadP RLE
readRLE = do
  _c <- get
  if isDigit _c
    then do
      _s <- munch isDigit
      let _len = read (_c:_s)
      _chr <- get
      return RLE { .. }
    else return RLE { _len = 1, _chr = _c }

instance Read RLE where
  readPrec = lift $ readRunRLE <++ readSingleRLE
  readListPrec = many readPrec

instance Show RLE where
  show RLE {..} = case _len of
    0 -> ""
    1 -> return _chr
    _ -> show _len ++ return _chr
  showList = (++) . foldMap show

encodeRLE :: ReadP RLE
encodeRLE = do
  c <- get
  s <- munch (c ==)
  return RLE { _len = succ $ genericLength s, _chr = c }

encodeRLEs :: ReadP [RLE]
encodeRLEs = many encodeRLE <* eof

decodeRLE :: RLE -> String
decodeRLE RLE {..} = replicate (fromIntegral _len) _chr

readMaybe' :: ReadP a -> String -> Maybe a
readMaybe' r = fmap (fst . fst) . uncons . readP_to_S r

decode :: String -> String
decode = foldMap decodeRLE . read @[RLE]

encode :: String -> String
encode = maybe "" (foldMap show) . readMaybe' encodeRLEs
