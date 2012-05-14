> module Euler.Problem10 where

> import Euler.Util

10. Calculate the sum of all the primes below two million.

> problem10 :: Integer
> problem10 = sum $ takeWhile (< 2000000) primes
