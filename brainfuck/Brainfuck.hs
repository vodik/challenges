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

type Cell = Word8
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

op :: (Memory t, Num c, Eq c) => Char -> Int -> Operation t c
op '>' n = shift R n
op '<' n = shift L n
op '+' n = alter $ incCell n
op '-' n = alter $ decCell n
op '.' n = output >*> n
op ',' 1 = io getInput >>= store
op ',' n = io getChar  >*> (n - 1) >> op ',' 1

eval :: (Memory t, Num c, Eq c) => Op -> Operation t c
eval (Op    x) = op x 1
eval (OpN n x) = op x n
eval (Loop  l) = let loop = brainfuck l >> whenValue loop in whenValue loop

brainfuck :: (Memory t, Num c, Eq c) => [Op] -> Operation t c
brainfuck = mapM_ eval

main :: IO ()
main = getArgs >>= parse >>= \code ->
    case parseBrainfuck code of
        Left err -> debug err
        Right xs -> do
            (out, mem) <- runMachine emptyMemory . brainfuck $ optimize xs
            putStrLn $ fromCell <$> out
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

sparseMemory :: Sparse Cell
sparseMemory = emptySparse 0

emptyMemory :: Tape Cell
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
