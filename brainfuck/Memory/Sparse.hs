module Memory.Sparse
    ( Sparse (..)
    , emptySparse
    ) where

import Control.Monad
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as M

import Memory

data Sparse a = Sparse a Int (Map Int a)
    deriving (Show, Read)

emptySparse :: Num a => a -> Sparse a
emptySparse d = Sparse d 0 M.empty

instance Memory Sparse where
    shift dir n (Sparse d i m) = let (+/-) = op dir in Sparse d (i +/- n) m

    value   (Sparse d i m) = fromMaybe d $ M.lookup i m
    alter f (Sparse d i m) = Sparse d i  $ M.alter (set' f d) i m

set' :: Eq a => (a -> a) -> a -> Maybe a -> Maybe a
set' f d v = do
    let v' = f $ fromMaybe d v
    guard (v' /= d) >> return v'

op :: Num a => Direction -> a -> a -> a
op L = (-)
op R = (+)
