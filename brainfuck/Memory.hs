module Memory where

class Memory t where
    right :: t a -> t a
    left  :: t a -> t a
    value :: (Num a) => t a -> a
    alter :: (Eq a, Num a) => (a -> a) -> t a -> t a
