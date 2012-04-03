Challenge 04
============

This challenge has us traverse a list of web pages on a wild goose
chase of sorts. We start on node `12345` and each subsequent page has
instructions on how to find the next node. To solve this I wrote a
very basic web crawler to automatically traverse the list.

While it really wasn't necessary to solve this challenge and
definitely lead to a longer solution, I decided to use monads to carry
around the immutable and mutable data for practise. This has lead to
a very clean solution.

> {-# LANGUAGE GeneralizedNewtypeDeriving #-}

> import Control.Applicative
> import Control.Monad.State
> import Control.Monad.Reader
> import Data.Char
> import Network.HTTP

> type Base = String
> type Node = Int

This is the definition of the `Traverse` monad. It caries around the
base url and tracks which node we're currently on.

> newtype Traverse a = Traverse (ReaderT Base (StateT Node IO) a)
>     deriving (Functor, Applicative, Monad, MonadIO, MonadReader String, MonadState Int)

> execTraverse :: Traverse a -> Base -> Node -> IO a
> execTraverse (Traverse a) base node = fst <$> (`runStateT` node) (runReaderT a base)

The base URL. The current node is passed in by setting `nothing`. For
example, the first node is `linkedlist.php?nothing=12345`.

> base :: Base
> base = "http://www.pythonchallenge.com/pc/def/linkedlist.php?nothing="

Construct the url by concattenating the base url with the current
node.

> url :: Traverse String
> url = liftA2 (++) ask (show <$> get)

Grab the contents of the current page. There is absolutely no error
handling here.

> access :: Traverse String
> access = url >>= io . simpleHTTP . getRequest >>= io . getResponseBody

Traverse the list. There only seems to be two kinds of responses to
handle: either we are told what the next node is, or we are told that
the next node is 1/2 the value of the current node. If we get
something else, we've most likely hit the end of the list and we
return the message.

> traverse :: Traverse String
> traverse = trace >> access >>= \rsp -> do
>     let next = last $ words rsp
>     case rsp of
>         "Yes. Divide by two and keep going." -> jump (`div` 2)
>         _ | all isDigit next -> follow next
>           | otherwise        -> return rsp
>   where
>     jump f   = modify f     >> traverse
>     follow n = put (read n) >> traverse
>     trace    = get >>= io . putStrLn . ("Travelling to node " ++) . show

> main :: IO ()
> main = execTraverse traverse base 12345 >>= putStrLn . ("Result: " ++)

> io :: (MonadIO m) => IO a -> m a
> io = liftIO
