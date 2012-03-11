module Memory.Tape where

import Memory

data Tape a = Tape a [a] [a]
    deriving (Show, Read)

instance Memory Tape where
    empty d = Tape d [] []

    right (Tape d l []    ) = Tape d (d:l) []
    right (Tape d l (r:rs)) = Tape d (r:l) rs

    left (Tape d []     r) = Tape d [] r
    left (Tape d (l:ls) r) = Tape d ls (l:r)

    value (Tape d l []    ) = d
    value (Tape d l (r:rs)) = r

    alter f (Tape d l []    ) = Tape d l [f d]
    alter f (Tape d l (r:rs)) = Tape d l (f r:rs)
