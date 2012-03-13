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

import Machine
import Brainfuck.Parser
import Brainfuck.Optimizer
import Memory.Tape
import Memory.Sparse

type Operation t c = Machine t c ()

toCell :: Num c => Char -> c
toCell = fromIntegral . ord

fromCell :: Integral c => c -> Char
fromCell = chr . fromIntegral

incCell :: Num c => Int -> c -> c
incCell = (+) . fromIntegral

decCell :: Num c => Int -> c -> c
decCell = subtract . fromIntegral

getInput :: Num c => IO c
getInput = do
    input <- try getChar
    case input of
        Left (SomeException _) -> return 0
        Right c                -> return $ toCell c

eval :: (Memory t, Num c, Eq c) => Char -> Int -> Operation t c
eval '>' n = shift R n
eval '<' n = shift L n
eval '+' n = alter $ incCell n
eval '-' n = alter $ decCell n
eval '.' n = output >*> n
eval ',' _ = io getInput >>= store

brainfuck :: (Memory t, Num c, Eq c) => [Op] -> Operation t c
brainfuck (Op    x:xs) = eval x 1 >> brainfuck xs
brainfuck (OpN n x:xs) = eval x n >> brainfuck xs
brainfuck (Loop  l:xs) = loop l   >> brainfuck xs
brainfuck []           = return ()

loop :: (Memory t, Num c, Eq c) => [Op] -> Operation t c
loop xs = let l = brainfuck xs >> whenValue l in whenValue l

main :: IO ()
main = getArgs >>= parse >>= \code ->
    case parseBrainfuck code of
        Left err -> debug err
        Right xs -> do
            (out, mem) <- runMachine emptyMemory . brainfuck $ optimize xs
            putStrLn $ map fromCell out
            debug mem
  where
    parse ["-h"] = usage   >> exit
    parse ["-v"] = version >> exit
    parse [fs]   = readFile fs
    parse []     = readLines
    parse _      = usage   >> exit

    usage   = putStrLn "Usage: bf [-vh] [file]"
    version = putStrLn "Brainfuck 0.1"
    exit    = exitSuccess
    die     = exitWith $ ExitFailure 1

sparseMemory :: Num c => Sparse c
sparseMemory = emptySparse 0

emptyMemory :: Num c => Tape c
emptyMemory = emptyTape False 0

debug :: Show a => a -> IO ()
debug = hPrint stderr

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readLines :: IO String
readLines = flushStr "# " >> getLine >>= \line ->
    if null line
        then return ""
        else (line ++) <$> readLines

io :: (MonadIO m) => IO a -> m a
io = liftIO
