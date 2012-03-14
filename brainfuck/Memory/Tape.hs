module Memory.Tape
    ( Tape (..)
    , emptyTape
    ) where

import Memory

data Tape a = Tape Bool a [a] [a]
    deriving (Show, Read)

emptyTape :: Bool -> a -> Tape a
emptyTape i d = Tape i d [] []

instance Memory Tape where
    shift L (Tape i d l r) = uncurry (Tape i d) $! left i d l r
    shift R (Tape i d l r) = uncurry (Tape i d) $! right  d l r

    value (Tape _ d _ []     ) = d
    value (Tape _ _ _ (r : _)) = r

    alter f (Tape i d l []      ) = f d `seq` Tape i d l [f d]
    alter f (Tape i d l (r : rs)) = f r `seq` Tape i d l $! (f r : rs)

left :: Bool -> a -> [a] -> [a] -> ([a], [a])
left _     _ (l : ls) r = (ls, l : r)
left True  d []       r = ([], d : r)
left False _ []       r = ([], r)

right :: a -> [a] -> [a] -> ([a], [a])
right d l []       = (d : l, [])
right _ l (r : rs) = (r : l, rs)
