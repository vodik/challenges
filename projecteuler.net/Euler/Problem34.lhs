> module Euler.Problem34 where

> import Euler.Util

34. Find the sum of all numbers which are equal to the sum of the
    factorial of their digits.

> factorialOfDigits = map (\x -> product [1..x]) . explode
>
> solve n = [ x | x <- [1..n], sum (factorialOfDigits x) == x ]
