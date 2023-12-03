module Shared where

import Control.Applicative (Alternative (..))
import Data.Char (isNumber)
import Data.Functor (void)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Void
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P hiding (space)
import Linear.V2 (V2(..))

type Parser a = P.Parsec Void Text a

parseFileMega :: FilePath -> Parser a -> IO a
parseFileMega filePath parser = do
  file <- T.readFile filePath
  case P.parse parser filePath file of
    Left err -> fail $ P.errorBundlePretty err
    Right a -> pure a

parseLines :: Parser a -> Parser [a]
parseLines p = do
  ([] <$ P.eof)
    <|> ( do
            a <- p
            P.newline
            as <- parseLines p
            pure (a : as)
        )

lexeme :: Parser a -> Parser a
lexeme = P.lexeme (void $ P.takeWhileP (Just "Whitespace") (== ' '))

chainl :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl term op = do
  x <- term
  rest x
  where
    rest x = do
      f <- op
      y <- term
      rest (f x y) <|> pure x

accumulateP :: a -> Parser (a -> a) -> Parser a
accumulateP initial parserFn = do
  (parserFn >>= (\fn -> accumulateP (fn initial) parserFn)) <|> pure initial

accumulateP1 :: a -> Parser (a -> a) -> Parser a
accumulateP1 initial parserFn = do
  fn <- parserFn
  accumulateP (fn initial) parserFn

many1 :: (Alternative f) => f a -> f [a]
many1 f = (:) <$> f <*> many f

numberP :: Parser Int
numberP = fmap (read . T.unpack) $ lexeme $ P.takeWhile1P (Just "Number") isNumber

gridParse :: String -> [(V2 Int, Char)]
gridParse str =
  let row_major = zip [0 ..] (zip [0 ..] <$> Prelude.lines str)
   in concatMap
        ( \(rowIndex, row) ->
            ( \(columnIndex, e) ->
                (V2 columnIndex rowIndex, e)
            )
              <$> row
        )
        row_major
