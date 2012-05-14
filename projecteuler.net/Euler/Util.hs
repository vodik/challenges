module Euler.Util where

import Control.Monad
import Data.List
import Data.Tuple

digitalRoot :: Integral a => a -> a
digitalRoot = (1 +) . (`mod` 9) . subtract 1

explode :: Integral a => a -> [a]
explode = unfoldr $ \x -> guard (x > 0) >> return (swap $ x `divMod` 10)

primes = 2 : filter ((== 1) . length . primeFactors) [3,5..]

-- primeFactors :: Integral a => a -> [a]
primeFactors n = factor n primes
  where
    factor n (p:ps)
        | p * p > n      = [n]
        | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
        | otherwise      = factor n ps

factors :: Integral a => a -> [a]
factors n = go n 2
  where
    go 1 _ = []
    go n f
        | n `mod` f == 0 = f : go (n `div` f) f
        | otherwise      = go n (f + 1)

divisors :: Integral a => a -> [a]
divisors y = [ x | x <- [1..y-1], y `mod` x == 0 ]
