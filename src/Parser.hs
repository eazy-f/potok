module Parser where

import qualified Data.ByteString.Char8 as B

newtype Parser a i = Parser {runParser :: i -> ParserState a i}
type ParserState a i = Either (a, i) (Parser a i)

instance Show (Parser a i) where
  show _ = "continuation"

readByte string parser = maybe stop proceed $ B.uncons string
  where
    stop = (Right $ Parser $ flip readByte parser)
    proceed = (\(byte, rest) -> parser byte rest)

parseWord s =
  readByte s (\b1 r1 ->
               readByte r1 (\b2 r2 -> Left ([b1, b2], r2)))

parse parser s = foldl (\p s -> p >>= (flip runParser s)) (Right $ Parser parser) s

evalParser msg parser state = either (parser . (flip mappend msg) . snd) (flip runParser msg) state

stop result remains = Left (result, remains)
