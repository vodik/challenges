> module Euler.Problem25 where

25. What is the first term in the Fibonacci sequence to contain 1000
    digits?

> fib :: Num a => [a]
> fib = 0 : 1 : zipWith (+) fib (tail fib)
>
> problem25 :: Integer
> problem25 = head $ dropWhile (< 10 ^ 999) fib
