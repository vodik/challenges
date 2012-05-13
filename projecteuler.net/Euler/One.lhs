Project Euler.net
=================

Solutions to the challenges presented on [project euler.net][euler]

  [euler]: http://projecteuler.net/problems

Challenges 1-50
---------------

> module Euler.One where
>
> import Control.Applicative
> import Control.Arrow
> import Control.Monad
> import Data.Array
> import Data.Char
> import Data.List
> import Data.Tuple
> import Data.Word

1. Add all the natural numbers below one thousand that are multiples
   of 3 or 5.

> problem01 :: Integer
> problem01 = sum [ x | x <- [1..999], x `mod` 3 == 0, x `mod` 5 == 0 ]

2. By considering the terms in the Fibonacci sequence whose values do
   not exceed four million, find the sum of the even-valued terms.

I can take advantage of the fact that even numbers only show up every
3rd position in the sequence. It is relatively straight forward to
generate a sequence of only every 3rd element.

$$
\begin{aligned}
  x_n &= x_{n-2} + x_{n-1}                                 \\
  x_n &= (x_{n-4} + x_{n-3}) + (x_{n-3} + x_{n-2})         \\
  x_n &= x_{n-4} + x_{n-3} + x_{n-3} + (x_{n-4} + x_{n-3}) \\
  x_n &= x_{n-4} + x_{n-4} + 3 x_{n-3}                     \\
  x_n &= (x_{n-6} + x_{n-5}) + x_{n-4} + 3 x_{n-3}         \\
  x_n &= x_{n-6} + 4 x_{n-3}                               \\
\end{aligned}
$$

I apply this here to generate a list of only the even Fibonacci
numbers.

> evenFib :: Num a => [a]
> evenFib = 0 : 2 : zipWith (\x y -> x + 4 * y) evenFib (tail evenFib)
>
> problem02 :: Integer
> problem02 = sum $ takeWhile (<= 4000000) evenFib

3. What is the largest prime factor of the number 600851475143 ?

> primes = 2 : filter ((== 1) . length . primeFactors) [3,5..]
>
> primeFactors n = factor n primes
>   where
>     factor n (p:ps)
>         | p * p > n      = [n]
>         | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
>         | otherwise      = factor n ps
>
> problem03 :: Integer
> problem03 = last (primeFactors 317584931803)

4. Find the largest palindrome made from the product of two 3-digit
   numbers.

> isPalindrome :: Eq a => [a] -> Bool
> isPalindrome x = x == reverse x
>
> problem04 :: Int
> problem04 = last [ sum | x <- [100..999], y <- [x..999]
>                        , let sum = x * y
>                        , isPalindrome $ show sum ]

5. What is the smallest number divisible by each of the numbers 1 to
   20?

> problem05 :: Integer
> problem05 = foldr1 lcm [1..20]

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

7. Find the 10001st prime.

> problem07 :: Integer
> problem07 = primes !! 1000

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

10. Calculate the sum of all the primes below two million.

> problem10 :: Integer
> problem10 = sum $ takeWhile (< 2000000) primes

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

12. What is the value of the first triangle number to have over five
    hundred divisors?

> triangle :: (Enum a, Num a) => [a]
> triangle = scanl1 (+) [1..]

> problem12 = triangle !! 10000

13. Find the first ten digits of the sum of one-hundred 50-digit
    numbers.

The 100 50-digit numbers are stored in `data/13.txt` for brevity's
sake

> numbers :: IO [Integer]
> numbers = fmap read . lines <$> readFile "data/13.txt"
>
> problem13 :: IO Integer
> problem13 = read . take 10 . show . sum <$> numbers

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

15. Starting in the top left corner in a 20 by 20 grid, how many
    routes are there to the bottom right corner?

>

16. What is the sum of the digits of the number $2^{1000}$?

> digitalRoot :: Integral a => a -> a
> digitalRoot = (1 +) . (`mod` 9) . subtract 1
>
> explode :: Integral a => a -> [a]
> explode = unfoldr (\x -> if x > 0 then Just . swap $ x `divMod` 10 else Nothing)

> problem16 :: Integer
> problem16 = sum . explode $ 2 ^ 1000

17. How many letters would be needed to write all the numbers in words
    from 1 to 1000?

>

18. Find the maximum sum travelling from the top of the triangle to
    the base.

>

19. How many Sundays fell on the first of the month during the
    twentieth century?

>

20. Find the sum of digits in $100!$

> problem20 :: Integer
> problem20 = sum . explode $ product [1..100]

21. Evaluate the sum of all amicable pairs under 10000.

>

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

23. Find the sum of all the positive integers which cannot be written
    as the sum of two abundant numbers.

>

24. What is the millionth lexicographic permutation of the digits 0,
    1, 2, 3, 4, 5, 6, 7, 8 and 9?

> problem24 :: Int
> problem24 = read $ permutations ['0'..'9'] !! 999999

25. What is the first term in the Fibonacci sequence to contain 1000
    digits?

> fib :: Num a => [a]
> fib = 0 : 1 : zipWith (+) fib (tail fib)
>
> problem25 :: Integer
> problem25 = head $ dropWhile (< 10 ^ 999) fib

26. Find the value of $d < 1000$ for which $\frac{1}{d}$ contains the
    longest recurring cycle.

>

27. Find a quadratic formula that produces the maximum number of
    primes for consecutive values of $n$.

>

28. What is the sum of both diagonals in a 1001 by 1001 spiral?

>

29. How many distinct terms are in the sequence generated by $a^b$ for
    $2 \le a \le 100$ and $2 \le b \le 100$?

>

30. Find the sum of all the numbers that can be written as the sum of
    fifth powers of their digits.

>

31. Investigating combinations of English currency denominations.

>

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

33. Discover all the fractions with an unorthodox cancelling method.

>

34. Find the sum of all numbers which are equal to the sum of the
    factorial of their digits.

> factorialOfDigits = map (\x -> product [1..x]) . explode
>
> solve n = [ x | x <- [1..n], sum (factorialOfDigits x) == x ]

35. How many circular primes are there below one million?

>

36. Find the sum of all numbers less than one million, which are
    palindromic in base 10 and base 2.

>

37. Find the sum of all eleven primes that are both truncatable from
    left to right and right to left.

>

38. What is the largest 1 to 9 pandigital that can be formed by
    multiplying a fixed number by 1, 2, 3, ... ?

>

39. If $p$ is the perimeter of a right angle triangle, $\{a, b, c\}$,
    which value, for $p \le 1000$, has the most solutions?

>

40. Finding the nth digit of the fractional part of the irrational
    number.

>

41. What is the largest n-digit pandigital prime that exists?

>

42. How many triangle words does the list of common English words
    contain?

>

43. Find the sum of all pandigital numbers with an unusual sub-string
    divisibility property.

>

44. Find the smallest pair of pentagonal numbers whose sum and
    difference is pentagonal.

>

45. After 40755, what is the next triangle number that is also
    pentagonal and hexagonal?

>

46. What is the smallest odd composite that cannot be written as the
    sum of a prime and twice a square?

>

47. Find the first four consecutive integers to have four distinct
    primes factors.

>

48. Find the last ten digits of $1^1 + 2^2 + ... + 1000^{1000}$.

> problem48 :: Integer
> problem48 = read . reverse . take 10 . reverse . show . sum $ join (^) <$> [1..1000]

49. Find arithmetic sequences, made of prime terms, whose four digits
    are permutations of each other.

>

50. Which prime, below one-million, can be written as the sum of the
    most consecutive primes?

>

