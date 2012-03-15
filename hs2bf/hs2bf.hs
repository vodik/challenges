import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer.Strict

data Memory = Memory
    { position :: Int }

data Op = Op Int Char
        | Loop [Op]
        deriving (Read, Show)

type Index = Int

data Cell = Cell Index
          deriving (Read, Show)

raw   = tell . fmap (Op 1)
cmd c = tell . return . (`Op` c)
loop  = censor $ return . Loop

goto (Cell new) = do
    pos <- subtract new <$> gets position
    when (pos /= 0) $ do
        if pos > 0
            then cmd '<' pos
            else cmd '>' $ abs pos
        modify $ \mem -> mem { position = new }

output x = goto x >> raw "."
zero   x = goto x >> loop (raw "-")

inc   x n = goto x >> cmd '+' n
dec   x n = goto x >> cmd '-' n
while x f = goto x >> loop (f >> dec x 1)

-- modify x y = do
--     t <- tmp
--     while y $ do
--        inc x
--        inc t
--     while t $
--        inc y

-- x `assign` y = zero x >> x `add` y

-- add = modify

emptyMem = Memory 0

render :: [Op] -> String
render = foldr toC mempty
  where
    toC (Op n x) xs = join [ replicate n x, xs ]
    toC (Loop x) xs = '[' : join [ render x, ']' : xs ]

main = do
    x <- (`evalStateT` emptyMem) . execWriterT $ do
        let a = Cell 4; b = Cell 9; c = Cell 8
        zero a
        zero b
        zero c
        output c
        output a
        while b $ do
            inc c 3
            output c
    putStrLn $ render x
