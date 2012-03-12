module Memory.Sparse where

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
    right (Sparse d i m) = Sparse d (1 + i) m
    left  (Sparse d i m) = Sparse d (1 `subtract` i) m
    value (Sparse d i m) = fromMaybe d $ M.lookup i m

    alter f (Sparse d i m) = Sparse d i $ M.alter f' i m
      where f' v = let v' = f $ fromMaybe d v in guard (v' /= d) >> return v'
