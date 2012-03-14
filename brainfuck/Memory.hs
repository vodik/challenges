module Memory
    ( Memory (..)
    , Direction (..)
    ) where

data Direction = L | R

class Memory t where
    shift :: Direction -> t a -> t a
    value :: (Num a) => t a -> a
    alter :: (Eq a, Num a) => (a -> a) -> t a -> t a
