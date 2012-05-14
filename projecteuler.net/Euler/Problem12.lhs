> module Euler.Problem12 where

12. What is the value of the first triangle number to have over five
    hundred divisors?

> triangle :: (Enum a, Num a) => [a]
> triangle = scanl1 (+) [1..]

> problem12 = triangle !! 10000
