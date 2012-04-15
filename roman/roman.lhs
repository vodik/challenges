> import Control.Applicative
> import Control.Monad.State

> class RomanNum a where
>    value :: Num b => a -> Maybe b

> instance RomanNum Char where
>    value 'I' = Just 1
>    value 'V' = Just 5
>    value 'X' = Just 10
>    value 'L' = Just 50
>    value 'C' = Just 100
>    value 'D' = Just 500
>    value 'M' = Just 1000
>    value _   = Nothing

> fromRoman' :: (RomanNum a, Num b, Ord b) => [a] -> b -> Maybe b
> fromRoman' []  acc = Just acc
> fromRoman' [x] acc = (acc +) <$> value x
> fromRoman' (a:b:xs) acc = do
>    a' <- value a
>    b' <- value b
>    if a' < b'
>        then fromRoman' xs (acc + b' - a')
>        else fromRoman' (b:xs) (acc + a')

> fromRoman :: (Num a, Ord a) => [Char] -> Maybe a
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
>     value (Repeatable n) = Just $ 1 * 10 ^ n
>     value (Once n)       = Just $ 5 * 10 ^ n

> toRoman :: String -> [Roman]
> toRoman str = (`evalStateT` 0) $ do
>     n <- roman <$> lift str
>     validate n
>     return n

> validate n = return ()

> safeRoman :: (Num a, Ord a) => [Char] -> Maybe a
> safeRoman = flip fromRoman' 0 . toRoman
