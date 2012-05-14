module Euler.Util where

import Control.Monad
import Data.List
import Data.Tuple

divides :: Integral a => a -> a -> Bool
x `divides` y = x `mod` y == 0

digitalRoot :: Integral a => a -> a
digitalRoot = (1 +) . (`mod` 9) . subtract 1

explode :: Integral a => a -> [a]
explode = unfoldr $ \x -> guard (x > 0) >> return (swap $ x `divMod` 10)

digitalSum :: Integral a => a -> a
digitalSum = sum . explode

primes :: Integral a => [a]
primes = 2 : [ x | x <- [3, 5..], length (primeFactors x) == 1 ]

primeFactors :: Integral a => a -> [a]
primeFactors n = go n primes
  where
    go n (p : ps)
        | p * p > n     = [n]
        | n `divides` p = p : go (n `div` p) (p : ps)
        | otherwise     = go n ps

divisors :: Integral a => a -> [a]
divisors y = 1 : [ x | x <- [2..y `div` 2], y `mod` x == 0 ]
