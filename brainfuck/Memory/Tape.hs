module Memory.Tape where

import Memory

data Tape a = Tape     a [a] [a]
            | Infinite a [a] [a]
    deriving (Show, Read)

emptyTape :: Bool -> a -> Tape a
emptyTape False d = Tape     d [] []
emptyTape True  d = Infinite d [] []

instance Memory Tape where
    right (Tape     d l []    ) = Tape     d (d:l) []
    right (Tape     d l (r:rs)) = Tape     d (r:l) rs
    right (Infinite d l []    ) = Infinite d (d:l) []
    right (Infinite d l (r:rs)) = Infinite d (r:l) rs

    left (Tape     d []     r) = Tape     d [] r
    left (Tape     d (l:ls) r) = Tape     d ls (l:r)
    left (Infinite d []     r) = Infinite d [] (d:r)
    left (Infinite d (l:ls) r) = Infinite d ls (l:r)

    value (Tape     d l []    ) = d
    value (Tape     d l (r:rs)) = r
    value (Infinite d l []    ) = d
    value (Infinite d l (r:rs)) = r

    alter f (Tape     d l []    ) = Tape     d l [f d]
    alter f (Tape     d l (r:rs)) = Tape     d l (f r:rs)
    alter f (Infinite d l []    ) = Infinite d l [f d]
    alter f (Infinite d l (r:rs)) = Infinite d l (f r:rs)
