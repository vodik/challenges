import Control.Applicative
import Control.Monad
import Data.Function
import Data.Char
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as M

type Cipher = Map Char Char

rotate :: [a] -> Int -> [a]
rotate xs n = take (length xs) . drop n $ cycle xs

cipher :: Int -> Cipher
cipher step = M.fromList $ alph `zip` rotate alph step
  where alph = ['a'..'z']

substitute :: Cipher -> Char -> Char
substitute = ap fromMaybe . flip M.lookup

main :: IO ()
main = fmap decipher . lines <$> getContents >>= mapM_ putStrLn
  where decipher = fmap . substitute . cipher $ step 'o' 'q'

step :: Char -> Char -> Int
step = abs .: on (-) ord

a .: b = (a .) . b
