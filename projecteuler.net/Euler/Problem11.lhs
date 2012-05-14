> module Euler.Problem11 where

> import Control.Applicative
> import Control.Arrow
> import Data.Array

11. What is the greatest product of four adjacent numbers on the same
    straight line in the 20 by 20 grid?

> type Index = (Int, Int)
> type Table = Array Index Int

First generate a immutable table of numbers.

> table :: IO Table
> table = listArray ((1, 1), (20, 20)) . map read . words <$> readFile "data/11.txt"

Here we define the transformations on coordinates to traverse the
table with.

> transformer :: [Index -> Index]
> transformer =
>     [ first  (+ 1)          -- horizontal
>     , second (+ 1)          -- vertical
>     , (+ 1) *** (+ 1)       -- right diagonal
>     , (+ 1) *** subtract 1  -- left diagonal
>     ]

Now calculate each possible set of indexes that represent products and
then calculate the actual products.

> transform :: (Index, Index) -> [[Index]]
> transform b = [ xs | i <- range b
>                    , t <- transformer
>                    , let xs = take 4 $ iterate t i
>                    , all (inRange b) xs ]
>
> products :: Array Index Int -> [Int]
> products t = [ product $ map (t !) s | s <- transform $ bounds t ]
>
> problem11 :: IO Int
> problem11 = maximum . products <$> table
