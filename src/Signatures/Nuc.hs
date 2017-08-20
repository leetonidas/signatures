module Signatures.Nuc
    ( 
        BBprop(..),
        Fprop(..),
        parseNuc
    ) where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String (Parser)
import Control.Monad
import Data.Char (isSpace)

data BBprop = BBst Int | BBcnt Int | BBanc [Int] | BBcall [Int] deriving (Show,Eq)
data Fprop = Fst [Int] | Fnm String | Fblk [[BBprop]] deriving (Show, Eq)

parseDec :: Parser Int
parseDec = read <$> many1 digit

parseHex :: Parser Int
parseHex = read . concat <$> sequence [string "0", string "x" <|> string "X", many1 hexDigit]

parseNum :: Parser Int
parseNum = try parseHex <|> parseDec

parseWord :: Parser String
parseWord = many1 (satisfy (not . isSpace))

parseSkipEnd :: Parser a -> Parser a
parseSkipEnd x = x <* sequence_ [skipMany (char ' '), void endOfLine]

parseBlockIntro :: Parser BBprop
parseBlockIntro = BBst <$> (count 2 tab *> parseSkipEnd (parseNum <* char ':'))

parseBlockLength :: Parser BBprop
parseBlockLength = BBcnt <$> (string "LENGTH:" >> spaces >> parseSkipEnd parseNum)

parseBlockAnc :: Parser BBprop
parseBlockAnc = BBanc <$> (string "ANC:" *> parseSkipEnd (many $ try (many (char ' ') *> parseNum)))

parseBlockCall :: Parser BBprop
parseBlockCall = BBcall <$> (string "CALLS:" *> parseSkipEnd (many $ try (many (char ' ') *> parseNum)))

parseBlockProp :: Parser BBprop
parseBlockProp = try (count 3 tab) >> choice [parseBlockLength, parseBlockAnc, parseBlockCall]

parseBlock :: Parser [BBprop]
parseBlock = parseBlockIntro >>= (\x -> (x:) <$> many parseBlockProp)

parseFunEntries :: Parser Fprop
parseFunEntries = Fst <$> (string "ENTRYS:" *> parseSkipEnd (many $ try (many (char ' ') *> parseNum)))

parseFunName :: Parser Fprop
parseFunName = Fnm <$> (string "NAME:" *> skipMany (char ' ') *> parseSkipEnd parseWord)

parseFunBlocks :: Parser Fprop
parseFunBlocks = Fblk <$> (parseSkipEnd (string "BLOCKS:") *> many1 parseBlock)

parseFun :: Parser [Fprop]
parseFun = string "fun_" *> manyTill anyChar endOfLine *> many (char '\t' *> choice [parseFunName, parseFunEntries, parseFunBlocks])

parseNuc :: Parser [[Fprop]]
parseNuc = many endOfLine *> many (parseFun <* many endOfLine)