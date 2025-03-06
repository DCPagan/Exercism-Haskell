{-# LANGUAGE LambdaCase #-}
module TwelveDays (recite) where

import Control.Lens
import Data.List

ordinal :: Integral a => a -> String
ordinal = \case
  1 -> "first"
  2 -> "second"
  3 -> "third"
  4 -> "fourth"
  5 -> "fifth"
  6 -> "sixth"
  7 -> "seventh"
  8 -> "eighth"
  9 -> "ninth"
  10 -> "tenth"
  11 -> "eleventh"
  12 -> "twelfth"
  _ -> ""

preamble :: Integral a => a -> String
preamble n
  = "On the " ++ ordinal n ++ " day of Christmas my true love gave to me: "

verses :: [String]
verses =
  [ "a Partridge in a Pear Tree"
  , "two Turtle Doves"
  , "three French Hens"
  , "four Calling Birds"
  , "five Gold Rings"
  , "six Geese-a-Laying"
  , "seven Swans-a-Swimming"
  , "eight Maids-a-Milking"
  , "nine Ladies Dancing"
  , "ten Lords-a-Leaping"
  , "eleven Pipers Piping"
  , "twelve Drummers Drumming"
  ]

stanza :: Integral a => a -> String
stanza n
  | n <= 0 = ""
  | otherwise = preamble n
    ++ (intercalate ", "
      $ ((filtered ((> 1) . length) . _last %~ ("and " ++))
        $ reverse $ take (fromIntegral n) verses))
    ++ "."

recite :: Int -> Int -> [String]
recite start stop = stanza <$> [start..stop]
