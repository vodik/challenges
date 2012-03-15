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

output x   = goto x >> raw "."
input  x   = goto x >> raw ","
inc    x n = goto x >> cmd '+' n
dec    x n = goto x >> cmd '-' n
zero   x   = goto x >> loop (raw "-")
while  x f = goto x >> loop (f >> dec x 1)

moveTo x y = while x $ inc y 1
copyTo x y = times x $ inc y 1

cell = do
    free <- gets nextFree
    modify $ \mem -> mem { nextFree = free + 1 }

    let c     = Cell free
        dirty = False

    when dirty $ zero c
    return c

temp  f = cell >>= f
store v = cell >>= \y -> inc y v    >> return y
copy  x = cell >>= \y -> copyTo x y >> return y
io      = cell >>= \y -> input y    >> return y

add x y = copy x >>= \c -> y `times` (inc c 1) >> return c
sub x y = copy x >>= \c -> y `times` (dec c 1) >> return c

times x f = temp $ \t -> while x (f >> inc t 1) >> moveTo t x

f <?> c = comment c >> f

emptyMem = Memory 0 0

render :: [Op] -> String
render = foldr toC mempty
  where
    toC (Op n    x) xs =        join [ replicate n x,   xs ]
    toC (Loop    x) xs = '['  : join [ render x,  ']' : xs ]
    toC (Comment c) xs = '\n' : join [ c,        '\n' : xs ]

printAll arr = mapM_ output arr

program = (`evalState` emptyMem) . execWriterT $ do
    v1 <- io
    v2 <- io
    v3 <- v1 `sub` v2       <?> "Subtract two values from each other"
    inc v3 (ord '0')        <?> "Convert num to ascii"
    printAll [ v1, v2, v3 ] <?> "Output all values"

main = putStrLn $ render program
