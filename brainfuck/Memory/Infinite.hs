module Memory.Infinite where

import Memory

data Infinite a = Infinite a [a] [a]
    deriving (Show, Read)

instance Memory Infinite where
    empty d = Infinite d [] []

    right (Infinite d l []    ) = Infinite d (d:l) []
    right (Infinite d l (r:rs)) = Infinite d (r:l) rs

    left (Infinite d []     r) = Infinite d [] (d:r)
    left (Infinite d (l:ls) r) = Infinite d ls (l:r)

    value (Infinite d l []    ) = d
    value (Infinite d l (r:rs)) = r

    alter f (Infinite d l []    ) = Infinite d l [f d]
    alter f (Infinite d l (r:rs)) = Infinite d l (f r:rs)
