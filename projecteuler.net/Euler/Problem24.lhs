> module Euler.Problem24 where

> import Data.List

24. What is the millionth lexicographic permutation of the digits 0,
    1, 2, 3, 4, 5, 6, 7, 8 and 9?

> problem24 :: Int
> problem24 = read $ permutations ['0'..'9'] !! 999999
