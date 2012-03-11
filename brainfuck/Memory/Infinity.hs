module Memory.Infinity where

import Memory

data Infinity a = Infinity a [a] [a]
    deriving (Show, Read)

instance Memory Infinity where
    empty d = Infinity d [] []

    right (Infinity d l []    ) = Infinity d (d:l) []
    right (Infinity d l (r:rs)) = Infinity d (r:l) rs

    left (Infinity d []     r) = Infinity d [] (d:r)
    left (Infinity d (l:ls) r) = Infinity d ls (l:r)

    value (Infinity d l []    ) = d
    value (Infinity d l (r:rs)) = r

    alter f (Infinity d l []    ) = Infinity d l [f d]
    alter f (Infinity d l (r:rs)) = Infinity d l (f r:rs)
