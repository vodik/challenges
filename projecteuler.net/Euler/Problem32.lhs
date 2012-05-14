> module Euler.Problem32 where

> import Control.Monad
> import Data.List
> import Euler.Util

32. Find the sum of all numbers that can be written as pandigital
    products.

> combs 0 xs = return ([], xs)
> combs n xs = do
>     y <- xs
>     (ys, rst) <- combs (n - 1) $ delete y xs
>     return (y:ys, rst)
>
> compress :: (Integral a) => [a] -> a
> compress = foldl' (\a b -> 10 * a + b) 0
>
> pandigitals :: [Int]
> pandigitals = nub $ do
>     (beg,end) <- combs 5 [1..9]
>     n <- [1,2]
>     let (a,b) = splitAt n beg
>         res = compress a * compress b
>     guard $ sort (explode res) == end
>     return res
>
> problem32 = sum pandigitals
