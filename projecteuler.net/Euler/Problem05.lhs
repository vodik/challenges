> module Euler.Problem05 where

5. What is the smallest number divisible by each of the numbers 1 to
   20?

> problem05 :: Integer
> problem05 = foldr1 lcm [1..20]
