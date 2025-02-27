{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Clock (addDelta, fromHourMin, toString) where

import Text.Printf
import Text.Read
import Text.Read.Lex
import qualified Text.ParserCombinators.ReadP as R
import qualified Text.ParserCombinators.ReadPrec as RP

data Clock where
  Clock :: {
    hours :: Int,
    minutes :: Int
  } -> Clock
  deriving (Eq, Ord)

fmt :: FieldFormat
fmt = FieldFormat {
  fmtWidth = Just 2,
  fmtPrecision = Nothing,
  fmtAdjust = Just ZeroPad,
  fmtSign = Nothing,
  fmtAlternate = False,
  fmtModifiers = "",
  fmtChar = 'd'
}

instance Read Clock where
  readPrec = do
    hours <- flip mod 24 <$> RP.lift readDecP
    _ <- RP.lift $ R.char ':'
    minutes <- flip mod 60 <$> RP.lift readDecP
    return $ Clock {..}

instance Show Clock where
  showsPrec _ Clock { hours, minutes } s
    = formatInt (mod hours 24) fmt ":" ++ formatInt (mod minutes 60) fmt s

fromHourMin :: Int -> Int -> Clock
fromHourMin hours minutes
  = Clock {
    hours = mod (hours + div minutes 60) 24,
    minutes = mod minutes 60
  }

toString :: Clock -> String
toString = show

addDelta :: Int -> Int -> Clock -> Clock
addDelta h m Clock {..}
  = Clock {
    hours = mod (hours + h + div (minutes + m) 60) 24,
    minutes = mod (minutes + m) 60
  }
