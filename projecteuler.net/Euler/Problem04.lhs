> module Euler.Problem04 where

4. Find the largest palindrome made from the product of two 3-digit
   numbers.

> isPalindrome :: Eq a => [a] -> Bool
> isPalindrome x = x == reverse x
>
> problem04 :: Int
> problem04 = last [ sum | x <- [100..999], y <- [x..999]
>                        , let sum = x * y
>                        , isPalindrome $ show sum ]
