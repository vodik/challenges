> import Control.Applicative
> import Control.Monad.State

> class RomanNum a where
>    value :: Num b => a -> b

> instance RomanNum Char where
>    value 'I' = 1
>    value 'V' = 5
>    value 'X' = 10
>    value 'L' = 50
>    value 'C' = 100
>    value 'D' = 500
>    value 'M' = 1000

> fromRoman' :: (RomanNum a, Num b, Ord b) => [a] -> b -> b
> fromRoman' []  acc = acc
> fromRoman' [x] acc = value x + acc
> fromRoman' (a:b:xs) acc =
>    let a' = value a
>        b' = value b
>    in if a' < b'
>        then fromRoman' xs (acc + b' - a')
>        else fromRoman' (b:xs) (acc + a')

> fromRoman :: (Num a, Ord a) => [Char] -> a
> fromRoman = flip fromRoman' 0



> data Roman = Repeatable Int
>            | Once Int
>            deriving (Show)

> roman :: Char -> Roman
> roman 'I' = Repeatable 0
> roman 'X' = Repeatable 1
> roman 'C' = Repeatable 2
> roman 'M' = Repeatable 3
> roman 'V' = Once 0
> roman 'L' = Once 1
> roman 'D' = Once 2

> instance RomanNum Roman where
>     value (Repeatable n) = 1 * 10 ^ n
>     value (Once n)       = 5 * 10 ^ n

> toRoman :: String -> [Roman]
> toRoman str = (`evalStateT` 0) $ do
>     n <- roman <$> lift str
>     validate n
>     return n

> validate n = return ()

> safeRoman :: (Num a, Ord a) => [Char] -> a
> safeRoman = flip fromRoman' 0 . toRoman
