module ResistorColors (Color(..), value) where

import Control.Arrow ((***), first)

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

value :: (Color, Color) -> Int
value = uncurry (+) . first (10 *) . (fromEnum *** fromEnum)
