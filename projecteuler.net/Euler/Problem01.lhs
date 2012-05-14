> module Euler.Problem01 where

1. Add all the natural numbers below one thousand that are multiples
   of 3 or 5.

> problem01 :: Integer
> problem01 = sum [ x | x <- [1..999], x `mod` 3 == 0, x `mod` 5 == 0 ]
