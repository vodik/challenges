module Memory.Classic where

import Memory

data Classic a = Classic Int a [a] [a]
    deriving (Show, Read)

instance Memory Classic where
    empty d = Classic 3000 d [] []

    right (Classic 1 d l rs    ) = Classic 1 d l rs
    right (Classic i d l []    ) = Classic (1 `subtract` i) d (d:l) []
    right (Classic i d l (r:rs)) = Classic (1 `subtract` i) d (r:l) rs

    left (Classic i d []     r) = Classic i d [] r
    left (Classic i d (l:ls) r) = Classic (1 + i) d ls (l:r)

    value (Classic i d l []    ) = d
    value (Classic i d l (r:rs)) = r

    alter f (Classic i d l []    ) = Classic i d l [f d]
    alter f (Classic i d l (r:rs)) = Classic i d l (f r:rs)
