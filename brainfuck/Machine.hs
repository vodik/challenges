{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Machine
    ( Memory
    , MachineT (..)
    , Direction (..)
    , runMachineT, runMachine
    , execMachineT, execMachine
    , shift, output
    , alter, value, store
    , whenValue, (>*>)
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer.Strict

import Memory (Memory, Direction (..))
import qualified Memory as M

newtype MachineT t c m a =
    MachineT { unMachineT :: WriterT [c] (StateT (t c) m) a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadWriter [c], MonadState (t c))

type Machine t c = MachineT t c Identity

instance MonadTrans (MachineT t c) where
    lift = MachineT . lift . lift

runMachineT :: (Functor m, Monad m, Memory t, Num c) => t c -> MachineT t c m a -> m ([c], t c)
runMachineT mem = (`runStateT` mem) . execWriterT . unMachineT

runMachine :: (Memory t, Num c) => t c -> Machine t c a -> ([c], t c)
runMachine mem = runIdentity . runMachineT mem

execMachineT :: (Functor m, Monad m, Memory t, Num c) => t c -> MachineT t c m a -> m [c]
execMachineT mem = (fst <$>) . runMachineT mem

execMachine :: (Memory t, Num c) => t c -> Machine t c a -> [c]
execMachine mem = fst . runMachine mem

shift :: (Functor m, Monad m, Memory t) => Direction -> Int -> MachineT t c m ()
shift d n = do
    mem <- get
    put $! M.shift d >* n $ mem

output :: (Functor m, Monad m, Memory t, Num c) => MachineT t c m ()
output = value >>= tell . return

alter :: (Functor m, Monad m, Memory t, Eq c, Num c) => (c -> c) -> MachineT t c m ()
alter = modify . M.alter

value :: (Functor m, Monad m, Memory t, Num c) => MachineT t c m c
value = M.value <$> get

store :: (Functor m, Monad m, Memory t, Eq c, Num c) => c -> MachineT t c m ()
store = alter . const

whenValue :: (Functor m, Monad m, Memory t, Eq c, Num c) => MachineT t c m () -> MachineT t c m ()
whenValue f = value >>= \v -> when (v /= 0) f

infix >*>, >*
(>*>) :: Monad m => m a -> Int -> m ()
(>*>) = flip replicateM_

(>*) :: (a -> a) -> Int -> a -> a
f >* n = foldl1 (.) $ replicate n f
