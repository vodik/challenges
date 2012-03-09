import Control.Applicative
import Data.Char
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S

type FrequencyMap = Map Char Int
type CharSet = Set Char

freqMap :: String -> FrequencyMap
freqMap = foldr (M.alter update) M.empty
  where update (Just x) = Just $ x + 1
        update Nothing  = Just 1

lowFreq :: FrequencyMap -> Int -> CharSet
lowFreq m c = S.fromList . fmap fst . M.toList $ M.filter (<= c) m

main :: IO ()
main = freqFilter <$> getContents >>= putStrLn
  where freqFilter = flip S.member . flip lowFreq 10 . freqMap >>= filter
