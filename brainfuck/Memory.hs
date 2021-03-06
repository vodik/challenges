module Memory
    ( Memory(..)
    , Direction(..)
    ) where

data Direction = L | R

class Memory t where
    shift :: Direction -> Int -> t a -> t a
    value :: Num a => t a -> a
    alter :: (Eq a, Num a) => (a -> a) -> t a -> t a
    set   :: (Eq a, Num a) => a -> t a -> t a
    set = alter . const
