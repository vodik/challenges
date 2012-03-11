module Memory.Sparse where

import Memory
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as M

data Sparse a = Sparse a Int (Map Int a)
    deriving (Show, Read)

instance Memory Sparse where
    empty d = Sparse 0 0 M.empty

    right (Sparse d i m) = Sparse d (1 + i) m
    left  (Sparse d i m) = Sparse d (1 `subtract` i) m
    value (Sparse d i m) = fromMaybe d $ M.lookup i m

    alter f (Sparse d i m) = Sparse d i $ M.alter f' i m
      where f' = Just . f . fromMaybe d
