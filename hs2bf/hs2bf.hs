import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer.Strict
import Data.Char

data Memory = Memory
    { position :: Int
    , nextFree :: Int
    }

data Op = Op Int Char
        | Loop [Op]
        | Comment String
        deriving (Read, Show)

type Index = Int

data Cell = Cell Index
          deriving (Read, Show)

raw   = tell . fmap (Op 1)
cmd c = tell . return . (`Op` c)
loop  = censor $ return . Loop

clean   = filter $ not . (`elem` "+-<>,.[]")
comment = tell . return . Comment . clean

goto (Cell new) = do
    pos <- gets $ subtract new . position
    when (pos /= 0) $ do
        if pos > 0
            then cmd '<' pos
            else cmd '>' $ abs pos
        modify $ \mem -> mem { position = new }

output x = goto x >> raw "."
input  x = goto x >> raw ","
zero   x = goto x >> loop (raw "-")

inc   x n = goto x >> cmd '+' n
dec   x n = goto x >> cmd '-' n
while x f = goto x >> loop (f >> dec x 1)

cell = do
    free <- gets nextFree
    modify $ \mem -> mem { nextFree = free + 1 }

    let c = Cell free
        dirty = False

    when dirty $ zero c
    return c

moveTo x y = while x $ inc y 1
copyTo x y = do
    t <- cell
    while x $ do
        inc t 1
        inc y 1
    t `moveTo` x

-- modify x y = do
--     t <- tmp
--     while y $ do
--        inc x
--        inc t
--     while t $ inc y

-- x `assign` y = zero x >> x `add` y

-- add = modify

emptyMem = Memory 0 0

render :: [Op] -> String
render = foldr toC mempty
  where
    toC (Op n x) xs = join [ replicate n x, xs ]
    toC (Loop x) xs = '[' : join [ render x, ']' : xs ]
    toC (Comment str) xs = '\n' : join [ str, "\n", xs ]

program = (`evalState` emptyMem) . execWriterT $ do
    v1 <- cell
    v2 <- cell
    comment "Hello World"
    input v1
    v1 `copyTo` v2
    output v1
    output v2

main = putStrLn $ render program
