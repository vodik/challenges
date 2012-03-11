module Main where

import Prelude hiding (catch)
import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Data.Char
import Data.Word
import System.Environment
import System.Exit
import System.IO

import Brainfuck.Parser
import Memory.Tape
import Machine

type BFMachine = Machine Tape Word8
type Operation = BFMachine ()

getInput :: IO Char
getInput = do
    input <- try getChar
    case input of
        Left (SomeException _) -> return '\0'
        Right c                -> return c

eval :: Char -> Operation
eval '>' = shiftRight
eval '<' = shiftLeft
eval '+' = alter (+ 1)
eval '-' = alter (subtract 1)
eval '.' = chr . fromEnum <$> value >>= putC
eval ',' = ord <$> io getInput >>= store . toEnum

brainfuck :: [Op] -> Operation
brainfuck (Op   x:xs) = eval x >> brainfuck xs
brainfuck (Loop l:xs) = loop l >> brainfuck xs
brainfuck []          = return ()

loop :: [Op] -> Operation
loop xs = let l = brainfuck xs >> whenValue l in whenValue l

main :: IO ()
main = getArgs >>= parse >>= \code ->
    case parseBrainfuck code of
        Left err -> debug err
        Right xs -> do
            (out, mem) <- runMachine (brainfuck xs)
            putStrLn out
            debug mem
  where
    parse ["-h"] = usage   >> exit
    parse ["-v"] = version >> exit
    parse [fs]   = readFile fs
    parse _      = readLines

    usage   = putStrLn "Usage: bf [-vh] [file]"
    version = putStrLn "Brainfuck 0.1"
    exit    = exitWith ExitSuccess
    die     = exitWith $ ExitFailure 1

debug :: Show a => a -> IO ()
debug = hPutStrLn stderr . show

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readLines :: IO String
readLines = flushStr "# " >> getLine >>= \line ->
    if null line
        then return ""
        else (line ++) <$> readLines

io :: (MonadIO m) => IO a -> m a
io = liftIO
