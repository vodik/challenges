Challenge 01
============

This challenge presents us with an encrypted message and a picture
which implies the message has been encoded with a Caesar cipher.

> import Control.Applicative
> import Control.Monad
> import Data.Function
> import Data.Char
> import Data.Map (Map)
> import Data.Maybe
> import qualified Data.Map as M

We'll represent the cipher as a map of characters onto characters.
This makes deciphering a string a simple matter of lookups.

> type Cipher = Map Char Char

Take a list and rotate is by `n` elements to the right. This function
helps us build the map we use to represent the cipher.

> rotate :: Int -> [a] -> [a]
> rotate n xs = take (length xs) . drop n $ cycle xs

Build a substitution cipher from the step size. We take various
character ranges we want to substitute over, in this case all lower and
capital letters, and zip this resulting list with a precisely rotated
version of itself and build a map out of it.

> cipher :: Int -> Cipher
> cipher step = M.fromList $ [lower, upper] >>= ap zip (rotate step)
>   where lower = ['a'..'z']
>         upper = ['A'..'Z']

Attempt to substitute letters. Since our cipher is nothing more than
just a map, lets look up the letter in the map, and should we fail
to find it return the input. This lets us substitute letters
efficiently while persevering punctuation.

> substitute :: Cipher -> Char -> Char
> substitute = ap fromMaybe . flip M.lookup

This deciphers the string. Its a simple matter of running substitute
over every single letter in the string.

> decipher :: Cipher -> String -> String
> decipher = map . substitute

> main :: IO ()
> main = decipher c <$> getContents >>= putStrLn
>   where c = cipher $ 'K' `step` 'M'

Calculate the difference between two letters since we given a hint
at the offset used for this cipher.

> step :: Char -> Char -> Int
> step = abs .: (-) `on` ord

> (.:) = (.) . (.)
