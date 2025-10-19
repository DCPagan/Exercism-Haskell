{-# OPTIONS_GHC -Wno-unused-matches #-}

module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import Data.Char
import Data.Function (on)

import System.Random.Stateful
import Control.Monad (replicateM)

encodeAlpha :: Char -> Int
encodeAlpha = subtract (ord 'a') . ord . toLower

decodeAlpha :: Int -> Char
decodeAlpha = chr . (ord 'a' +)

caesarDecode :: String -> String -> String
caesarDecode = zipWith f . cycle
  where
    f k t = decodeAlpha $ mod (on (-) encodeAlpha t k) 26

caesarEncode :: String -> String -> String
caesarEncode = zipWith f . cycle
  where
    f k t = decodeAlpha $ mod (on (+) encodeAlpha t k) 26

caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
  gen <- initStdGen >>= newIOGenM
  key <- replicateM (length text) (uniformRM ('a', 'z') gen)
  return (key, caesarEncode key text)
