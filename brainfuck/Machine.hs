{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Machine
    ( Memory
    , Machine (..)
    , runMachine
    , execMachine
    , shiftLeft, shiftRight, output
    , alter, value, store
    , whenValue, (>*>)
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer

import Memory (Memory)
import qualified Memory as M

newtype Machine m s a = Machine (WriterT [s] (StateT (m s) IO) a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadWriter [s], MonadState (m s))

runMachine :: (Memory m, Num s) => Machine m s a -> IO ([s], m s)
runMachine (Machine a) = runStateT (execWriterT a) (M.empty 0)

execMachine :: (Memory m, Num s) => Machine m s a -> IO [s]
execMachine = (fst <$>) . runMachine

shiftLeft :: Memory m => Machine m s ()
shiftLeft = modify M.left

shiftRight :: Memory m => Machine m s ()
shiftRight = modify M.right

output :: (Memory m, Num s) => Machine m s ()
output = value >>= tell . return

alter :: (Memory m, Num s) => (s -> s) -> Machine m s ()
alter = modify . M.alter

value :: (Memory m, Num s) => Machine m s s
value = M.value <$> get

store :: (Memory m, Num s) => s -> Machine m s ()
store = alter . const

whenValue :: (Memory m, Eq s, Num s) => Machine m s () -> Machine m s ()
whenValue f = value >>= \v -> when (v /= 0) f

infix >*>
(>*>) :: Monad m => m a -> Int -> m ()
(>*>) = flip replicateM_
