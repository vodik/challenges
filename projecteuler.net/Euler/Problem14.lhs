> module Euler.Problem14 where

> import Data.Word

14. Find the longest sequence using a starting number under one
    million.

The following iterative sequence is defined for the set of positive
integers:

$$
  n(x) = \left\{ \begin{array}{ll}
    \frac n 2 & \mbox{if $x$ is even} \\
    3n + 1    & \mbox{if $x$ is odd}  \\
  \end{array} \right.
$$

> n :: Integral a => a -> a
> n x | even x    = x `div` 2
>     | otherwise = 3 * x + 1
>
> chain :: Int -> Word32 -> Int
> chain c 1 = c
> chain c x = chain (c + 1) $ n x
>
> pmax :: (Int, Word32) -> Word32 -> (Int, Word32)
> pmax x n = x `max` (chain 1 n, n)
>
> problem14 :: (Int, Word32)
> problem14 = foldl pmax (1, 1) [1..10^6]
