module Machine
    ( Memory
    , MachineT(..)
    , Direction(..)
    , State(..)
    , runMachine, execMachine, execMachineT
    , input, output, halt
    , shift, alter, value, store
    , whenValue
    ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Identity
import Data.Monoid
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as S

import Memory (Memory, Direction(..))
import qualified Memory as M

data State w t = State { out :: Seq w, mem :: t w }

data Result w t m a = Halt   (State w t) (MachineT w t m a)
                    | Input  (State w t) (w -> MachineT w t m a)
                    | Result (State w t) a

newtype MachineT w t m a =
    MachineT { runMachineT :: State w t -> m (Result w t m a) }

type Machine w t = MachineT w t Identity

instance Monad m => Functor (MachineT w t m) where
    fmap = liftM

instance Monad m => Monad (MachineT w t m) where
    return a = MachineT $ \s -> return $ Result s a

    f >>= g = MachineT $ \s -> do
        rst <- runMachineT f s
        case rst of
            Halt   s' c -> return . Halt  s' $ c >>= g
            Input  s' c -> return . Input s' $ c >=> g
            Result s' a -> runMachineT (g a) s'

    fail = MachineT . const . fail

instance MonadTrans (MachineT w t) where
    lift m = MachineT $ \s -> liftM (Result s) m

instance MonadIO m => MonadIO (MachineT w t m) where
    liftIO = lift . liftIO

runMachine :: MachineT w t Identity a -> State w t -> Result w t Identity a
runMachine s = runIdentity . runMachineT s

execMachineT :: Monad m => t w -> (Seq w -> t w -> m ()) -> m w -> MachineT w t m a -> m (Seq w, t w)
execMachineT t cb i f = run f (State mempty t)
  where
    run f state = do
        rst <- runMachineT f state
        case rst of
            (Halt   s cont)   -> cb (out s) (mem s) >> run cont s
            (Input  s cont)   -> i >>= \w -> run (cont w) s
            (Result s result) -> return (out s, mem s)

execMachine :: Num w => t w -> [w] -> Machine w t a -> (Seq w, t w)
execMachine t i f = runIdentity $ run f (i `mappend` repeat 0) (State mempty t)
  where
    run f i@(x:xs) state = do
        rst <- runMachineT f state
        case rst of
            (Halt   s cont)   -> run cont i s
            (Input  s cont)   -> run (cont x) xs s
            (Result s result) -> return (out s, mem s)

get :: (Memory t, Monad m) => MachineT w t m (t w)
get = MachineT $ \s -> return $ Result s (mem s)

gets :: (Memory t, Monad m) => (t w -> a) -> MachineT w t m a
gets f = MachineT $ \s -> return $ Result s (f (mem s))

put :: (Memory t, Monad m) => t w -> MachineT w t m ()
put v = MachineT $ \s -> return $ Result (State (out s) v) ()

tell :: Monad m => w -> MachineT w t m ()
tell v = MachineT $ \s -> return $ Result (State (out s |> v) (mem s)) ()

halt :: Monad m => MachineT w t m ()
halt = MachineT $ \s -> return . Halt s $ MachineT (\s' -> return $ Result s' ())

input :: Monad m => MachineT w t m w
input = MachineT $ \s -> return $ Input s (\i -> MachineT $ \s' -> return $ Result s' i)

modify :: (Monad m, Memory t) => (t w -> t w) -> MachineT w t m ()
modify f = get >>= (put $!) . f

output :: (Monad m, Memory t, Num w) => MachineT w t m ()
output = value >>= tell

shift :: (Monad m, Memory t) => Direction -> Int -> MachineT w t m ()
shift = (modify .) . M.shift

alter :: (Monad m, Memory t, Eq w, Num w) => (w -> w) -> MachineT w t m ()
alter = modify . M.alter

value :: (Monad m, Memory t, Num w) => MachineT w t m w
value = gets M.value

store :: (Monad m, Memory t, Eq w, Num w) => w -> MachineT w t m ()
store = alter . const

whenValue :: (Monad m, Memory t, Eq w, Num w) => MachineT w t m () -> MachineT w t m ()
whenValue f = value >>= \v -> when (v /= 0) f
