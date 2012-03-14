module Main where

import Prelude hiding (catch)
import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Char
import Data.Word
import System.Environment
import System.Exit
import System.IO

import Machine
import Brainfuck.Parser
import Brainfuck.Optimizer
import Memory.Tape
import Memory.Sequence
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

safeChar :: Num c => IO c
safeChar = either (\(SomeException _) -> 0) toCell <$> try getChar

op :: (Memory t, Num c, Eq c) => Char -> Int -> Operation t c
op '>' n = shift R n
op '<' n = shift L n
op '+' n = alter $ incCell n
op '-' n = alter $ decCell n
op '.' n = output >*> n
op ',' 1 = io safeChar >>= store
op ',' n = io getChar  >*> (n - 1) >> op ',' 1

eval :: (Memory t, Num c, Eq c) => Op -> Operation t c
eval (Op    x) = op x 1
eval (OpN n x) = op x n
eval (Loop  l) = let loop = brainfuck l >> whenValue loop in whenValue loop

brainfuck :: (Memory t, Num c, Eq c) => [Op] -> Operation t c
brainfuck = foldl1 (>>) . fmap eval

run :: [Op] -> IO ()
run code = do
    (out, mem) <- runMachine emptyMemory . brainfuck $ optimize code
    putStrLn $ fromCell <$> out
    debug mem

main :: IO ()
main = getArgs >>= parse >>= either debug run . parseBrainfuck
  where
    parse ["-h"] = usage   >> exit
    parse ["-v"] = version >> exit
    parse [file] = readFile file
    parse []     = readLines
    parse _      = usage   >> exit

    usage   = getProgName >>= \name -> putStrLn $ join [ "Usage: ", name, " [-vh] [file]" ]
    version = putStrLn "Brainfuck 0.1"
    exit    = exitSuccess

sparseMemory :: Sparse Cell
sparseMemory = emptySparse 0

seqMemory :: Sequence Cell
seqMemory = emptySequence False 0

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
