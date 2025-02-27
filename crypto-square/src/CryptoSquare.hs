{-# LANGUAGE TupleSections #-}
module CryptoSquare where

import Data.Char
import Data.Function
import Data.List
import Data.Monoid

normalize :: String -> String
normalize =
  fmap toLower
    . filter (getAll . (All . not . isSpace <> All . not . isPunctuation))

getWidth :: Integral a => String -> a
getWidth = ceiling . sqrt . genericLength

getRectanglePad :: Integral a => String -> a
getRectanglePad "" = 0
getRectanglePad s = mod <$> negate . genericLength <*> getWidth $ s

padRectangle :: Integral a => a -> String -> String
padRectangle x s = s ++ replicate (fromIntegral x) ' '

toRows :: Integral a => a -> String -> [String]
toRows width
  = fmap (fmap snd) . groupBy (on (==) $ flip div width . fst)
    . zip (enumFrom 0)

encode :: String -> String
encode = intercalate " " . transpose . (getWidth >>= toRows)
  . (getRectanglePad >>= padRectangle) . normalize
