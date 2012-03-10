module Main where

import Prelude hiding (catch)
import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Data.Char
import System.Environment
import System.Exit
import System.IO

import Brainfuck.Parser
import Machine

type Operation = Machine ()

getInput :: Machine Char
getInput = do
    input <- io $ try getChar
    case input of
        Left (SomeException _) -> return '\0'
        Right c                -> return c

eval :: Char -> Operation
eval '>' = shiftRight
eval '<' = shiftLeft
eval '+' = alter (+ 1)
eval '-' = alter (subtract 1)
eval '.' = chr <$> value    >>= putC
eval ',' = ord <$> getInput >>= store

brainfuck :: [Op] -> Operation
brainfuck (Op   x:xs) = eval x >> brainfuck xs
brainfuck (Loop l:xs) = loop l >> brainfuck xs
brainfuck []          = return ()

loop :: [Op] -> Operation
loop xs = let l = brainfuck xs >> whenValue l in whenValue l

main :: IO ()
main = getArgs >>= parse >>= \code ->
    case parseBrainfuck code of
        Left err -> print err
        Right xs -> runMachine (brainfuck xs) >>= print
        -- Right xs -> execMachine (brainfuck xs) >>= putStrLn
  where
    parse ["-h"] = usage   >> exit
    parse ["-v"] = version >> exit
    parse [fs]   = readFile fs
    parse _      = getContents

    usage   = putStrLn "Usage: bf [-vh] [file]"
    version = putStrLn "Brainfuck 0.1"
    exit    = exitWith ExitSuccess
    die     = exitWith $ ExitFailure 1

io :: (MonadIO m) => IO a -> m a
io = liftIO
