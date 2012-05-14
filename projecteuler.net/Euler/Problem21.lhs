Problem 21
==========

> module Euler.Problem21 where
>
> import Control.Applicative
> import Euler.Util

Let $d(n)$ be defined as the sum of proper divisors of $n$ (numbers
less than $n$ which divide evenly into $n$).

If $d(a) = b$ and $d(b) = a$, where $a \neq b$, then $a$ and $b$ are an
amicable pair and each of $a$ and $b$ are called amicable numbers.

For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20,
22, 44, 55 and 110; therefore $d(220) = 284$. The proper divisors of
284 are 1, 2, 4, 71 and 142; so $d(284) = 220$.

Evaluate the sum of all the amicable numbers under $10000$.

> sumOfDivisors :: Integral a => a -> a
> sumOfDivisors = sum . divisors

> amicablePairs :: Integral a => [(a, a)]
> amicablePairs = [ (y, x) | x <- [1..]
>                          , let y = sumOfDivisors x
>                          , y < x
>                          , x == sumOfDivisors y ]

> problem21 :: Int
> problem21 = sum $ uncurry (+) <$> takeWhile ((< 10000) . snd) amicablePairs
