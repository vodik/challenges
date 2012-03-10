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
cipher step = M.fromList $ [lower, upper] >>= zip `ap` (`rotate` step)
  where lower = ['a'..'z']
        upper = ['A'..'Z']

substitute :: Cipher -> Char -> Char
substitute = ap fromMaybe . flip M.lookup

main :: IO ()
main = fmap decipher . lines <$> getContents >>= mapM_ putStrLn
  where decipher = fmap . substitute . cipher $ 'o' `step` 'q'

step :: Char -> Char -> Int
step = abs .: (-) `on` ord

(.:) = (.) . (.)
