{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Data.Char
import Network.HTTP

type Base = String
type Node = Int

newtype Traverse a = Traverse (ReaderT Base (StateT Node IO) a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader String, MonadState Int)

baseUrl :: Base
baseUrl = "http://www.pythonchallenge.com/pc/def/linkedlist.php?nothing="

execTraverse :: Traverse a -> Base -> Node -> IO a
execTraverse (Traverse a) base node = fst <$> runStateT (runReaderT a base) node

url :: Traverse String
url = liftA2 (++) ask (show <$> get)

access :: Traverse String
access = url >>= io . simpleHTTP . getRequest >>= io . getResponseBody

traverse :: Traverse String
traverse = trace >> access >>= \rsp -> do
    let next = last $ words rsp
    case rsp of
        "Yes. Divide by two and keep going." -> jump (`div` 2)
        _ | all isDigit next -> follow next
          | otherwise        -> return rsp
  where
    jump f   = modify f     >> traverse
    follow n = put (read n) >> traverse
    trace    = get >>= io . putStrLn . ("Travelling to node " ++) . show

main :: IO ()
main = execTraverse traverse baseUrl 12345 >>= putStrLn . ("Result: " ++)

io :: (MonadIO m) => IO a -> m a
io = liftIO
