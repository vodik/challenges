module Brainfuck.Parser
    ( Op (..)
    , parseBrainfuck
    ) where

import Control.Applicative hiding ((<|>))
import Text.ParserCombinators.Parsec

data Op = Op   Char
        | OpN  Int Char
        | Loop [Op]
        deriving (Eq, Show, Read)

parseBrainfuck :: String -> Either ParseError [Op]
parseBrainfuck = parse (opsTill eof) ""

junk :: Parser ()
junk = skipMany $ noneOf "+-<>.,[]#"

opsTill :: Parser a -> Parser [Op]
opsTill f = junk *> manyTill (ops <* junk) f

ops :: Parser Op
ops = try op <|> loop

op :: Parser Op
op = Op <$> oneOf "+-<>.,#" <?> "operator"

loop :: Parser Op
loop = do
    char '['
    Loop <$> opsTill (char ']') <?> "loop"
