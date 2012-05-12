Roman to Arabic Numeral Converter
=================================

> import Control.Applicative
> import Control.Monad.State
> import Data.Maybe

> class Numeral a where
>    value :: Num b => a -> Maybe b

An obvious instance of our numeral class is with Char. This allows us
to directly evaluate strings as mkNumeral numerals.

> instance Numeral Char where
>    value 'I' = Just 1
>    value 'V' = Just 5
>    value 'X' = Just 10
>    value 'L' = Just 50
>    value 'C' = Just 100
>    value 'D' = Just 500
>    value 'M' = Just 1000
>    value  _  = Nothing

Evaluate a mkNumeral numeral representation. Evaluation is simple,
look at two elements at a time. If the left element is less than the
right element, add the difference between the two. Otherwise only add
the left most value to the output.

> calculate :: (Numeral a, Num b, Ord b) => [a] -> b -> Maybe b
> calculate [ ] 0   = Nothing
> calculate [ ] acc = Just acc
> calculate [x] acc = (acc +) <$> value x
> calculate (a:b:xs) acc = do
>    a' <- value a
>    b' <- value b
>    if a' < b'
>        then calculate xs     $ acc + b' - a'
>        else calculate (b:xs) $ acc + a'

This is the naive implementation. By directly evaluating a string we
do no validation. This will correctly evaluate any well-formed
mkNumeralrnumerals. However it will also evaluate many non-well-formed
numerals that fit the assumptions above, such as `IC` as 99 and `XXVM`
as 1015.

> fromRoman :: (Num a, Ord a) => String -> Maybe a
> fromRoman = flip calculate 0

Validation
----------

Since the above version is very naive in its evaluation, lets see if
we can come up with something only evaluates well-formed Roman
numerals.

From [wikipedia][wiki]:

- The symbols `I`, `X`, `C`, and `M` can be repeated three times in
  succession, but no more. (They may appear more than three times if
  they appear non-sequentially, such as `XXXIX`.) `D`, `L`, and `V`
  can never be repeated.
- `I` can be subtracted from `V` and `X` only. `X` can be subtracted
  from `L` and `C` only. `C` can be subtracted from `D` and `M` only.
  `V`, `L`, and `D` can never be subtracted.
- Only one small-value symbol may be subtracted from any large-value
  symbol.

  [wiki]: http://en.wikipedia.org/wiki/Roman_numerals

> data Roman = Repeat Int
>            | Once Int
>            deriving (Show)

> instance Numeral Roman where
>     value (Repeat n) = return $ 1 * 10 ^ n
>     value (Once n)   = return $ 5 * 10 ^ n

> data Parser

> mkNumeral :: Char -> Maybe Roman
> mkNumeral 'I' = Just $ Repeat 0
> mkNumeral 'X' = Just $ Repeat 1
> mkNumeral 'C' = Just $ Repeat 2
> mkNumeral 'M' = Just $ Repeat 3
> mkNumeral 'V' = Just $ Once 0
> mkNumeral 'L' = Just $ Once 1
> mkNumeral 'D' = Just $ Once 2
> mkNumeral  _  = Nothing

> subtractable (Once   _) _          = False
> subtractable (Repeat a) (Repeat b) = b - a == 1
> subtractable (Repeat a) (Once   b) = b - a == 0

> convert :: String -> Maybe [Roman]
> convert = mapM mkNumeral

> safeRoman :: (Num a, Ord a) => String -> Maybe a
> safeRoman = convert >=> flip calculate 0
