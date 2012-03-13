module Brainfuck.Optimizer ( optimize ) where

import Brainfuck.Parser

compress :: [Op] -> [Op]
compress (Loop l:xs) = Loop (compress l) : compress xs
compress (op:xs)     = let (l, r) = break (/= op) xs in merge l op : compress r
compress []          = []

merge :: [Op] -> Op -> Op
merge xs (Op x) = OpN (length xs + 1) x

optimize :: [Op] -> [Op]
optimize = compress
