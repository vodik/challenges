> module Euler.Problem48 where

> import Control.Applicative
> import Control.Monad

48. Find the last ten digits of $1^1 + 2^2 + ... + 1000^{1000}$.

> problem48 :: Integer
> problem48 = read . reverse . take 10 . reverse . show . sum $ join (^) <$> [1..1000]
