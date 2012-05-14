module Main where

import Control.Applicative
import Control.Monad
import System.Environment
import System.FilePath
import Problems

withIO :: Int -> IO Result
withIO p = Just <$> readFile ("data" </> show p) >>= problem p

runIO :: Int -> IO Result
runIO p = problem p Nothing

main :: IO ()
main = getArgs >>= mapM_ ((print =<<) . runIO . read)
