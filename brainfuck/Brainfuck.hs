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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8

import Machine
import Brainfuck.Parser
import Brainfuck.Optimizer
import Memory.Tape
import Memory.Sparse

type Cell = Word8
type BFMachine = Machine Tape Cell
type Operation = BFMachine ()

getInput :: IO Cell
getInput = do
    input <- try getChar
    case input of
        Left (SomeException _) -> return 0
        Right c                -> return . toEnum $ ord c

incWord :: Int -> Cell -> Cell
incWord = (+) . toEnum

decWord :: Int -> Cell -> Cell
decWord = subtract . toEnum

eval :: Char -> Int -> Operation
eval '>' n = shiftRight >*> n
eval '<' n = shiftLeft  >*> n
eval '+' n = alter $ incWord n
eval '-' n = alter $ decWord n
eval '.' n = output >*> n
eval ',' _ = io getInput >>= store

brainfuck :: [Op] -> Operation
brainfuck (Op    x:xs) = eval x 1 >> brainfuck xs
brainfuck (OpN n x:xs) = eval x n >> brainfuck xs
brainfuck (Loop  l:xs) = loop l   >> brainfuck xs
brainfuck []           = return ()

loop :: [Op] -> Operation
loop xs = let l = brainfuck xs >> whenValue l in whenValue l

main :: IO ()
main = getArgs >>= parse >>= \code ->
    case parseBrainfuck code of
        Left err -> debug err
        Right xs -> do
            (out, mem) <- runMachine . brainfuck $ optimize xs
            B8.putStrLn $ BS.pack out
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
