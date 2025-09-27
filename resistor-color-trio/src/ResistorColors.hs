{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module ResistorColors (Color(..), Resistor(..), label, ohms) where

import Data.Function (on)

data Color
  = Black
  | Brown
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Violet
  | Grey
  | White
  deriving (Eq, Show, Enum, Bounded, Ord)

newtype Resistor = Resistor { bands :: (Color, Color, Color) }
  deriving (Eq, Show, Bounded, Ord)

{--
 - Given a decimal exponent. return a new exponent and an SI prefix.
 -}
siPrefix :: Int -> (Int, String)
siPrefix x = case div x 3 of
  -9 -> (x + 27, "ronto")
  -8 -> (x + 24, "yocto")
  -7 -> (x + 21, "zepto")
  -6 -> (x + 18, "atto")
  -5 -> (x + 15, "femto")
  -4 -> (x + 12, "pico")
  -3 -> (x + 9, "nano")
  -2 -> (x + 6, "micro")
  -1 -> (x + 3, "milli")
  0 -> (x, "")
  1 -> (x - 3, "kilo")
  2 -> (x - 6, "mega")
  3 -> (x - 9, "giga")
  4 -> (x - 12, "tera")
  5 -> (x - 15, "peta")
  6 -> (x - 18, "exa")
  7 -> (x - 21, "zetta")
  8 -> (x - 24, "yotta")
  9 -> (x - 27, "ronna")
  _
    | x < -9 -> (x + 30, "quecto")
    | otherwise -> (x - 30, "quetta")

value :: Color -> Color -> Int
value = on ((+) . (10 *)) fromEnum

label :: Resistor -> String
label
  Resistor {bands = (a, b, c)} = show v ++ zeropad ++ " " ++ prefix ++ "ohms"
  where
    v' = value a b
    (v, e') = if mod v' 10 == 0 then (div v' 10, succ c) else (v', c)
    (e, prefix) = siPrefix $ fromEnum e'
    zeropad = if v == 0 then "" else replicate e '0'

ohms :: Resistor -> Int
ohms Resistor {bands = (a, b, c)} = value a b * 10 ^ fromEnum c
