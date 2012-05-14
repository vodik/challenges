module Euler.Util where

import Data.List
import Data.Tuple

digitalRoot :: Integral a => a -> a
digitalRoot = (1 +) . (`mod` 9) . subtract 1

explode :: Integral a => a -> [a]
explode = unfoldr (\x -> if x > 0 then Just . swap $ x `divMod` 10 else Nothing)

primes = 2 : filter ((== 1) . length . primeFactors) [3,5..]

primeFactors n = factor n primes
  where
    factor n (p:ps)
        | p * p > n      = [n]
        | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
        | otherwise      = factor n ps
