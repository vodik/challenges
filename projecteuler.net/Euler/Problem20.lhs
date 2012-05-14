> module Euler.Problem20 where

> import Euler.Util

20. Find the sum of digits in $100!$

> problem20 :: Integer
> problem20 = sum . explode $ product [1..100]
