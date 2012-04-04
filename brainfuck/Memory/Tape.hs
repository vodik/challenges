module Memory.Tape
    ( Tape(..)
    , emptyTape
    ) where

import Memory

data Tape a = Tape Bool a [a] [a]
    deriving (Show, Read)

emptyTape :: Bool -> a -> Tape a
emptyTape i d = Tape i d [] []

instance Memory Tape where
    shift L n (Tape i d l r) = uncurry (Tape i d) $! left i n d (l, r)
    shift R n (Tape i d l r) = uncurry (Tape i d) $! right  n d (l, r)

    value (Tape _ d _ []     ) = d
    value (Tape _ _ _ (r : _)) = r

    alter f (Tape i d l []      ) = Tape i d l [f d]
    alter f (Tape i d l (r : rs)) = Tape i d l $! (f r : rs)

left :: Bool -> Int -> a -> ([a], [a]) -> ([a], [a])
left _     0 d lst         = lst
left i     n d (l : ls, r) = left i     (n - 1) d (ls, l : r)
left True  n d ([],     r) = left True  (n - 1) d ([], d : r)
left False n d ([],     r) = left False (n - 1) d ([], r)

right :: Int -> a -> ([a], [a]) -> ([a], [a])
right 0 d lst         = lst
right n d (l, []    ) = right (n - 1) d (d : l, [])
right n d (l, r : rs) = right (n - 1) d (r : l, rs)
