> module Euler.Problem22 where

> import Control.Applicative
> import Data.Char
> import Data.List

22. What is the total of all the name scores in the file of first
    names?

The data is almost in a format which Haskell's read can understand,
lets be cheeky and use read to parse the array.

> names :: IO [String]
> names = sort . read . mkArray <$> readFile "data/names.txt"
>   where mkArray = ("[" ++) . (++ "]")
>
> score :: String -> Int
> score = sum . map (subtract (ord '@') . ord . toUpper)
>
> problem22 :: IO Int
> problem22 = sum . zipWith (\x y -> x * score y) [1..] <$> names
