import Control.Applicative
import Data.Char
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S

freqMap :: String -> Map Char Int
freqMap = foldr (M.alter update) M.empty
  where update (Just x) = Just $ x + 1
        update Nothing  = Just 1

lowFreq :: Map Char Int -> Int -> Set Char
lowFreq m c = S.fromList . fmap fst . M.toList $ M.filter (not . (> c)) m

main :: IO ()
main = getContents >>= \str ->
    let freq = freqMap str
        set  = lowFreq freq 10
    in putStrLn $ filter (`S.member` set) str
