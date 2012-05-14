> module Euler.Problem09 where

> import Control.Applicative
> import Data.List

9. Find the only Pythagorean triplet, $\{a, b, c\}$, for which
   $a + b + c = 1000$.

There exists exactly one Pythagorean triplet for which $a + b + c = 1000$.
Find the product $abc$.

> triplets :: Integral a => a -> [[a]]
> triplets n = [ [a, b, round c] | a <- [1..n], b <- [a..n]
>                                , let c = sqrt . fromIntegral $ a ^ 2 + b ^ 2
>                                , fromIntegral (round c) == c ]
>
> problem09 :: Maybe Integer
> problem09 = product <$> find ((== 1000) . sum) (triplets 375)
