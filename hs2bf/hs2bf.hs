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

raw  = tell . fmap (Op 1)
loop = censor $ return . Loop

goto (Cell new) = do
    pos <- gets position
    case pos - new of
        x | x > 0 -> left x
          | x < 0 -> right $ abs x
          | otherwise -> return ()
    modify $ \mem -> mem { position = new }
  where
    left  = tell . return . flip Op '<'
    right = tell . return . flip Op '>'

output x = goto x >> raw "."
zero   x = goto x >> reset
reset    = loop $ raw "-"

-- while x f = goto x >> loop (f >> dec x)

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
render code = foldr toC "" code
  where
    toC (Op n x) xs = replicate n x ++ xs
    toC (Loop x) xs = '[' : render x ++ "]" ++ xs

main = do
    x <- (`evalStateT` emptyMem) . execWriterT $ do
        -- a <- store 5
        -- b <- store 9
        -- a `add` b
        -- output a
        --
        zero (Cell 4)
        zero (Cell 9)
        zero (Cell 8)
        output (Cell 8)
        output (Cell 4)

    putStrLn "DONE..."
    putStrLn $ render x
