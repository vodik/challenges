{-# LANGUAGE ExistentialQuantification #-}

module Problems where

import Euler.Problem01
import Euler.Problem02
import Euler.Problem03
import Euler.Problem04
import Euler.Problem05
import Euler.Problem06
import Euler.Problem07
import Euler.Problem08
import Euler.Problem09
import Euler.Problem10
import Euler.Problem11
import Euler.Problem12
import Euler.Problem13
import Euler.Problem14
import Euler.Problem16
import Euler.Problem20
import Euler.Problem22
import Euler.Problem24
import Euler.Problem25
import Euler.Problem32
import Euler.Problem34
import Euler.Problem48

data Result = forall s. Show s => Result s

instance Show Result where
    show (Result r) = show r

problem :: Int -> Maybe String -> IO Result
problem  1 = const . return $ Result problem01
problem  2 = const . return $ Result problem02
problem  3 = const . return $ Result problem03
problem  4 = const . return $ Result problem04
problem  5 = const . return $ Result problem05
problem  6 = const . return $ Result problem06
problem  7 = const . return $ Result problem07
problem  8 = undefined
problem  9 = const . return $ Result problem09
problem 10 = const . return $ Result problem10
problem 11 = undefined
problem 12 = const . return $ Result problem12
problem 13 = undefined
problem 14 = const . return $ Result problem14
problem 15 = undefined
problem 16 = const . return $ Result problem16
problem 17 = undefined
problem 18 = undefined
problem 19 = undefined
problem 20 = const . return $ Result problem20
problem 21 = undefined
problem 22 = undefined
problem 23 = undefined
problem 24 = const . return $ Result problem24
problem 25 = const . return $ Result problem25
problem 26 = undefined
problem 27 = undefined
problem 28 = undefined
problem 29 = undefined
problem 30 = undefined
problem 31 = undefined
problem 32 = const . return $ Result problem32
problem 33 = undefined
problem 34 = undefined
problem 35 = undefined
problem 36 = undefined
problem 37 = undefined
problem 38 = undefined
problem 39 = undefined
problem 40 = undefined
problem 41 = undefined
problem 42 = undefined
problem 43 = undefined
problem 44 = undefined
problem 45 = undefined
problem 46 = undefined
problem 47 = undefined
problem 48 = const . return $ Result problem48
problem 49 = undefined
problem 50 = undefined
problem  _ = fail "invalid problem"
