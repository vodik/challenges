Challenge 03
============

We are looking for each letter in the centre of the pattern:
`xXXXxXXXx` (where `x` represents any lower case letter and `X` any
upper case letter).

It seems like the author expected this challenge to be solved with
regular expressions. I have chosen, for practise, to attempt a
solution with Parsecs instead.

> import Control.Applicative hiding ((<|>), many)
> import Control.Monad
> import Data.Maybe
> import Text.ParserCombinators.Parsec

> parser :: Parser String
> parser = catMaybes <$> manyTill bodyguard eof

> bodyguard :: Parser (Maybe Char)
> bodyguard = Just <$> try body <|> eof *> return Nothing
>                               <|> anyChar *> bodyguard

Using Control.Applicative's `(<*)` and `(*>)` operators, we can
return the middle character while defining the entire expression to
attempt to parse.

> body :: Parser Char
> body = lower *> block *> lower <* block <* lower

A block is nothing more than any three uppercase letters in a row.

> block :: Parser String
> block = replicateM 3 upper

> main :: IO ()
> main = getContents >>= either print putStrLn . parse parser ""
