> module Euler.Problem08 where

> import Control.Applicative
> import Data.Char

8. Discover the largest product of five consecutive digits in the
   1000-digit number.

> number :: IO [Int]
> number = map digitToInt . filter isDigit <$> readFile "data/08.txt"

> groups :: Int -> [a] -> [[a]]
> groups c xs
>     | length xs >= c = let (l,r) = splitAt c xs in l : groups c (drop 1 l ++ r)
>     | otherwise      = []
>
> problem08 :: IO Int
> problem08 = maximum . map product . groups 5 <$> number
