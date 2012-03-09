import Control.Applicative hiding ((<|>), many)
import Control.Monad
import Data.Maybe
import Text.ParserCombinators.Parsec

parser :: Parser String
parser = catMaybes <$> manyTill bodyguard eof

bodyguard :: Parser (Maybe Char)
bodyguard = Just <$> try body <|> eof *> return Nothing
                              <|> anyChar *> bodyguard

body :: Parser Char
body = lower *> block *> lower <* block <* lower

block :: Parser String
block = replicateM 3 upper

main :: IO ()
main = do
    contents <- getContents
    case parse parser "" contents of
        Left err -> print err
        Right xs -> putStrLn xs
