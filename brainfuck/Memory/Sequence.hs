{-# LANGUAGE ViewPatterns #-}

module Memory.Sequence
    ( Sequence (..)
    , emptySequence
    ) where

import Data.Sequence (Seq, (<|), viewl, ViewL (..))
import qualified Data.Sequence as S
import Memory

data Sequence a = Sequence Bool a (Seq a) (Seq a)
    deriving (Show, Read)

emptySequence :: Bool -> a -> Sequence a
emptySequence i d = Sequence i d S.empty S.empty

instance Memory Sequence where
    shift L (Sequence i d l r) = uncurry (Sequence i d) $! left i d l r
    shift R (Sequence i d l r) = uncurry (Sequence i d) $! right  d l r

    value (Sequence _ _ _ (viewl -> r :< _)) = r
    value (Sequence _ d _ (viewl -> EmptyL)) = d

    alter f (Sequence i d l (viewl -> r :< rs)) = Sequence i d l $! f r <| rs
    alter f (Sequence i d l (viewl -> EmptyL )) = Sequence i d l $! S.singleton (f d)

left :: Bool -> a -> Seq a -> Seq a -> (Seq a, Seq a)
left _     _ (viewl -> l :< ls) r = (ls,      l <| r)
left True  d (viewl -> EmptyL ) r = (S.empty, d <| r)
left False _ (viewl -> EmptyL ) r = (S.empty, r)

right :: a -> Seq a -> Seq a -> (Seq a, Seq a)
right _ l (viewl -> r :< rs) = (r <| l, rs)
right d l (viewl -> EmptyL ) = (d <| l, S.empty)
