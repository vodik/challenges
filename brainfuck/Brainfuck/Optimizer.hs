module Brainfuck.Optimizer where

import Control.Monad
import Data.Monoid

import Brainfuck.Parser

compress :: [Op] -> [Op]
compress (Op x:xs) =
    let c = length $ takeWhile (== Op x) xs
    in OpN (c + 1) x : compress (drop c xs)
compress (Loop l:xs) = Loop (compress l) : compress xs
compress []     = []

optimize :: [Op] -> [Op]
optimize = compress
