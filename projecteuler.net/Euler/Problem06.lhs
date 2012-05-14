> module Euler.Problem06 where

> import Control.Applicative

6. What is the difference between the sum of the squares and the
   square of the sums?

Find the difference between the sum of the squares of the first one
hundred natural numbers and the square of the sum.

> squareOfSum :: Integral a => a -> a
> squareOfSum x = (^ 2) $ sum [1..x]
>
> sumOfSquares :: Integral a => a -> a
> sumOfSquares x = sum $ (^ 2) <$> [1..x]
>
> problem06 :: Integer
> problem06 = squareOfSum 100 - sumOfSquares 100
