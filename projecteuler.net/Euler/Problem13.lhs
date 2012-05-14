> module Euler.Problem13 where

> import Control.Applicative

13. Find the first ten digits of the sum of one-hundred 50-digit
    numbers.

The 100 50-digit numbers are stored in `data/13.txt` for brevity's
sake

> numbers :: IO [Integer]
> numbers = fmap read . lines <$> readFile "data/13.txt"
>
> problem13 :: IO Integer
> problem13 = read . take 10 . show . sum <$> numbers
