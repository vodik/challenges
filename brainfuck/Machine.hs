{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Machine
    ( Memory
    , Machine (..)
    , Direction (..)
    , runMachine
    , execMachine
    , shift, output
    , alter, value, store
    , whenValue, (>*>)
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer.Strict

import Memory (Memory)
import qualified Memory as M

data Direction = L | R

newtype Machine t c a = Machine (WriterT [c] (StateT (t c) IO) a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadWriter [c], MonadState (t c))

runMachine :: (Memory t, Num c) => t c -> Machine t c a -> IO ([c], t c)
runMachine mem (Machine a) = runStateT (execWriterT a) mem

execMachine :: (Memory t, Num c) => t c -> Machine t c a -> IO [c]
execMachine mem = (fst <$>) . runMachine mem

shift :: Memory t => Direction -> Int -> Machine t c ()
shift L = modify . flip run M.left
shift R = modify . flip run M.right

output :: (Memory t, Num c) => Machine t c ()
output = value >>= tell . return

alter :: (Memory t, Eq c, Num c) => (c -> c) -> Machine t c ()
alter = modify . M.alter

value :: (Memory t, Num c) => Machine t c c
value = M.value <$> get

store :: (Memory t, Eq c, Num c) => c -> Machine t c ()
store = alter . const

whenValue :: (Memory t, Eq c, Num c) => Machine t c () -> Machine t c ()
whenValue f = value >>= \v -> when (v /= 0) f

infix >*>
(>*>) :: Monad m => m a -> Int -> m ()
(>*>) = flip replicateM_

run :: Int -> (a -> a) -> a -> a
run n f = foldr (.) id $ replicate n f
