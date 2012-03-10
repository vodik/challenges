module Main where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer
import Data.Char

import Brainfuck.Parser
import Machine
import Zipper

type Operation = Machine ()

inc :: (Num a) => Zipper a -> Zipper a
inc = alter (+ 1)

dec :: (Num a) => Zipper a -> Zipper a
dec = alter $ subtract 1

showValue :: Operation
showValue = chr . value <$> get >>= tell . return

readValue :: Operation
readValue = ord <$> io getChar >>= modify . store

cmd :: Char -> Operation
cmd '>' = modify right
cmd '<' = modify left
cmd '+' = modify inc
cmd '-' = modify dec
cmd '.' = showValue
cmd ',' = readValue

brainfuckMachine :: String -> Operation
brainfuckMachine = mconcat . map cmd

main :: IO ()
-- main = getLine >>= runMachine . brainfuckMachine >>= print
main = do
    program <- parseBrainfuck <$> getLine
    case program of
        Left err -> print err
        Right xs -> print xs

io :: (MonadIO m) => IO a -> m a
io = liftIO
