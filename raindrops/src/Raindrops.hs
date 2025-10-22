module Raindrops (convert) where

pling :: Int -> String
pling n = if mod n 3 == 0 then "Pling" else ""

plang :: Int -> String
plang n = if mod n 5 == 0 then "Plang" else ""

plong :: Int -> String
plong n = if mod n 7 == 0 then "Plong" else ""

convert :: Int -> String
convert n
  | gcd n (3 * 5 * 7) == 1 = show n
  | otherwise = pling <> plang <> plong $ n
