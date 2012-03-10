module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Data.Char

import Brainfuck.Parser
import Machine

type Operation = Machine ()

whenValue :: Operation -> Operation
whenValue f = value >>= \v -> when (v /= 0) f

cmd :: Char -> Operation
cmd '>' = shiftRight
cmd '<' = shiftLeft
cmd '+' = alter (+ 1)
cmd '-' = alter $ subtract 1
cmd '.' = chr <$> value      >>= tell . return
cmd ',' = ord <$> io getChar >>= store

brainfuck :: [Op] -> Operation
brainfuck (Op   x:xs) = cmd  x >> brainfuck xs
brainfuck (Loop l:xs) = loop l >> brainfuck xs
brainfuck []          = return ()

loop :: [Op] -> Operation
loop xs = let l = brainfuck xs >> whenValue l in whenValue l

main :: IO ()
main = do
    program <- parseBrainfuck <$> getContents
    case program of
        Left err -> print err
        Right xs -> runMachine (brainfuck xs) >>= print
        -- Right xs -> execMachine (brainfuck xs) >>= putStrLn

io :: (MonadIO m) => IO a -> m a
io = liftIO
