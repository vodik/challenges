module Zipper where

data Zipper a = Zipper a [a] [a]
    deriving (Show, Read)

empty :: a -> Zipper a
empty d = Zipper d [] []

right :: Zipper a -> Zipper a
right (Zipper d l []    ) = Zipper d (d:l) []
right (Zipper d l (r:rs)) = Zipper d (r:l) rs

left :: Zipper a -> Zipper a
left (Zipper d []     r) = Zipper d [] (d:r)
left (Zipper d (l:ls) r) = Zipper d ls (l:r)

value :: Zipper a -> a
value (Zipper d l []    ) = d
value (Zipper d l (r:rs)) = r

alter :: (a -> a) -> Zipper a -> Zipper a
alter f (Zipper d l []    ) = Zipper d l [f d]
alter f (Zipper d l (r:rs)) = Zipper d l (f r:rs)

store :: a -> Zipper a -> Zipper a
store = alter . const
