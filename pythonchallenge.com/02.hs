import Control.Applicative
import Data.Char
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S

type FrequencyMap = Map Char Int
type CharSet = Set Char

freqMap :: String -> FrequencyMap
freqMap = foldr (M.alter count) M.empty
  where count (Just x) = Just $ x + 1
        count Nothing  = Just 1

lowFreq :: FrequencyMap -> Int -> CharSet
lowFreq m c = S.fromList . fmap fst . M.toList $ M.filter (<= c) m

main :: IO ()
main = freqFilter <$> getContents >>= putStrLn
  where freqFilter = flip S.member . (`lowFreq` 10) . freqMap >>= filter
