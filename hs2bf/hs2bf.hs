{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Applicative
import Control.Monad.Writer.Strict

newtype Gen a = Gen { unGen :: Writer [String] a }
    deriving (Functor, Applicative, Monad, MonadWriter [String])

data Cell a = Cell { value :: a }
    deriving (Read, Show)

instance Num a => Num (Cell a) where
    (Cell x) + (Cell y) = Cell $ x + y
    (Cell x) * (Cell y) = Cell $ x * y
    abs         = Cell . abs . value
    signum      = Cell . signum . value
    fromInteger = Cell . fromInteger

data Op a = Cell a :+ Cell a
          | Cell a :- Cell a
          deriving (Read, Show)

data Command = Command String

(-->) :: (Show a, Num a) => Op a -> Cell a -> Gen ()
(x :+ y) --> cell = cell `assign` value (x + y)
(x :- y) --> cell = cell `assign` value (x - y)

assign :: (Show a, Num a) => Cell a -> a -> Gen ()
assign cell value = do
    tell $ return msg
  where
    msg = "Putting " ++ show value ++ " in " ++ show cell

output :: (Show a, Num a) => Cell a -> Gen ()
output cell = do
    tell $ return msg
  where
    msg = "Outputting value: " ++ show (value cell)

store :: Int -> Gen (Cell Int)
store n = do
    tell $ return msg
    return $ Cell n
  where
    msg = "Storing " ++ show n ++ " in a new cell"

brainfuck :: Gen ()
brainfuck = do
    a <- store 7
    b <- store 9
    a :+ b --> b
    output b

main = mapM_ putStrLn . execWriter $ unGen brainfuck
