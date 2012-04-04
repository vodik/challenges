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
import qualified Data.Foldable as F
import qualified Data.Sequence as S

import Brainfuck.Optimizer
import Brainfuck.Parser
import Machine
import Memory.Tape
import Memory.Sparse

type Cell = Word8
type Operation w t = MachineT w t IO ()

toCell :: Num a => Char -> a
toCell = fromIntegral . ord

fromCell :: Integral a => a -> Char
fromCell = chr . fromIntegral

incCell :: Num a => Int -> a -> a
incCell = flip (+) . fromIntegral

decCell :: Num a => Int -> a -> a
decCell = flip (-) . fromIntegral

safeChar :: Num a => IO a
safeChar = either (\(SomeException _) -> 0) toCell <$> try getChar

op :: (Memory t, Num w, Eq w) => Char -> Int -> Operation w t
op '>' n = shift R n
op '<' n = shift L n
op '+' n = alter $ incCell n
op '-' n = alter $ decCell n
op '.' n = output >*> n
op ',' 1 = input >>= store
op ',' n = input >> op ',' (n - 1)
op '#' n = halt

eval :: (Memory t, Num w, Eq w) => Op -> Operation w t
eval (Op    x) = op x 1
eval (OpN n x) = op x n
eval (Loop  l) = let loop = brainfuck l >> whenValue loop in whenValue loop

brainfuck :: (Memory t, Num w, Eq w) => [Op] -> Operation w t
brainfuck = foldl1 (>>) . fmap eval

run :: [Op] -> IO ()
run code = do
    (out, mem) <- execMachineT tapeMemory halted safeChar . brainfuck $ optimize code
    putStrLn $ fromCell <$> F.toList out
    debug mem
  where
    halted _ mem = putStrLn "HALT!" >> debug mem

main :: IO ()
main = getArgs >>= parse >>= either debug run . parseBrainfuck
  where
    parse ["-h"] = usage   >> exit
    parse ["-v"] = version >> exit
    parse [file] = hSetBuffering stdin NoBuffering >> readFile file
    parse []     = readLines
    parse _      = usage   >> exit

    usage   = getProgName >>= \name -> putStrLn $ join [ "Usage: ", name, " [-vh] [file]" ]
    version = putStrLn "Brainfuck 0.0.2"
    exit    = exitSuccess

sparseMemory :: Sparse Cell
sparseMemory = emptySparse 0

tapeMemory :: Tape Cell
tapeMemory = emptyTape False 0

debug :: Show a => a -> IO ()
debug = hPrint stderr

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readLines :: IO String
readLines = flushStr "# " >> getLine >>= \line ->
    if null line
        then return ""
        else (line ++) <$> readLines

infix >*>
(>*>) :: Monad m => m a -> Int -> m ()
(>*>) = flip replicateM_
