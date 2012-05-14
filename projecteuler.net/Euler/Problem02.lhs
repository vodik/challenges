> module Euler.Problem02 where

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
