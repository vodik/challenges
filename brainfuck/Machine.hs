{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Machine
    ( Memory
    , Machine (..)
    , runMachine
    , execMachine
    , shiftLeft, shiftRight
    , alter, value, store
    ) where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer

import Zipper (Zipper)
import qualified Zipper as Z

type Memory = Zipper Int

newtype Machine a = Machine (WriterT String (StateT Memory IO) a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadWriter String, MonadState Memory)

runMachine :: Machine a -> IO (String, Memory)
runMachine (Machine a) = runStateT (execWriterT a) (Z.empty 0)

execMachine :: Machine a -> IO String
execMachine a = fst <$> runMachine a

shiftLeft :: Machine ()
shiftLeft = modify Z.left

shiftRight :: Machine ()
shiftRight = modify Z.right

alter :: (Int -> Int) -> Machine ()
alter = modify . Z.alter

value :: Machine Int
value = Z.value <$> get

store :: Int -> Machine ()
store = alter . const
