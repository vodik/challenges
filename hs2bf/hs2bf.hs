import Control.Monad
import Control.Monad.State
import Control.Monad.Writer

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
    pos <- gets position
    case pos - new of
        0 -> return ()
        x | x > 0 -> cmd '<' x
          | x < 0 -> cmd '>' $ abs x
    modify $ \mem -> mem { position = new }

output x = goto x >> raw "."
zero   x = goto x >> reset
reset    = loop $ raw "-"

while x f = goto x >> loop (f >> goto x >> raw "-")

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
            output c
            output a

    putStrLn "DONE..."
    putStrLn $ render x

(.:) = (.) . (.)
