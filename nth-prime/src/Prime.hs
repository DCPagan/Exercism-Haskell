module Prime (nth) where

sieve :: Integral a => [a] -> [a]
sieve (p:xs) = p : (sieve $ filter ((> 0) . flip mod p) xs)

nth :: Int -> Maybe Integer
nth n = if n < 1 then Nothing else Just $ sieve (enumFrom 2) !! pred n
