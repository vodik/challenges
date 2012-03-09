import Control.Applicative
import Control.Monad
import Data.Function
import Data.Char
import Data.Maybe
import qualified Data.Map as M

rotate :: [a] -> Int -> [a]
rotate xs n = take (length xs) . drop n $ cycle xs

cipher :: Char -> Char -> [(Char, Char)]
cipher a b = alph `zip` rotate alph (a /- b)
  where
    alph = ['a'..'z']
    (/-) = abs .: on (-) ord

main :: IO ()
main = decipher . lines <$> getContents >>= mapM_ putStrLn
  where
    cMap     = M.fromList $ cipher 'o' 'q'
    decipher = fmap . fmap . ap fromMaybe $ flip M.lookup cMap

a .: b = (a .) . b
