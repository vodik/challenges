> module Euler.Problem16 where

> import Euler.Util

16. What is the sum of the digits of the number $2^{1000}$?

> problem16 :: Integer
> problem16 = sum . explode $ 2 ^ 1000
