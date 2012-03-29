Challenge 02
============

For this challenge we are presented with a large block of characters
and the challenge is to find the rare characters. To solve this
challenge we first calculate the frequency of each letter and then
filter out any letter that occurs too frequently.

> import Control.Applicative
> import Data.Char
> import Data.Map (Map)
> import Data.Set (Set)
> import System.Environment
> import qualified Data.Map as M
> import qualified Data.Set as S

Some types to help us: `FrequencyMap` to store the frequency of the
letters and `CharSet` to represent a set of letters.

> type FrequencyMap = Map Char Int
> type CharSet = Set Char

Here we calculate the frequency of each letter in a message.

> freqMap :: String -> FrequencyMap
> freqMap = foldr (M.alter count) M.empty
>   where count (Just x) = Just $ x + 1
>         count Nothing  = Just 1

Now build a set containing only those characters in the frequency map
who fall below a threshold.

> lowFreqSet :: Int -> FrequencyMap -> CharSet
> lowFreqSet c = M.keysSet . M.filter (<= c)

Here we do the filtering. First it builds a set of all characters
which do not appear frequently in the string and then filters out all
*other* characters which are *not* members of this set.

> freqFilter :: Int -> String -> String
> freqFilter f = flip S.member . lowFreqSet f . freqMap >>= filter

> main :: IO ()
> main = getArgs >>= parse >>= \f ->
>     freqFilter f <$> getContents >>= putStrLn
>   where
>     parse :: [String] -> IO Int
>     parse []    = return 10
>     parse (x:_) = return $ read x
