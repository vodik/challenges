module Brainfuck.Parser (Op (..), parseBrainfuck) where

import Control.Applicative hiding ((<|>))
import Text.ParserCombinators.Parsec

data Op = Op Char
        | Loop [Op]
        deriving (Eq, Show, Read)

parseBrainfuck :: String -> Either ParseError [Op]
parseBrainfuck = parse (opsTill eof) ""

opsTill :: Parser a -> Parser [Op]
opsTill = manyTill ops

ops :: Parser Op
ops = try op <|> try loop
  where
    op   = Op <$> oneOf "+-<>.,"
    loop = char '[' >> Loop <$> opsTill (char ']')
