{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Machine where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer

import Zipper

type Memory = Zipper Int

newtype Machine a = Machine (WriterT String (StateT Memory IO) a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadWriter String, MonadState Memory)

instance Monoid a => Monoid (Machine a) where
    mempty  = return mempty
    mappend = liftM2 mappend

runMachine :: Machine a -> IO (String, Memory)
runMachine (Machine a) = runStateT (execWriterT a) (emptyZipper 0)

execMachine :: Machine a -> IO String
execMachine a = fst <$> runMachine a
