module Shared where

import Control.Applicative (Alternative (..))
import Control.Monad (forM_)
import Data.Char (isAlphaNum, isNumber)
import Data.Functor (void)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Void
import Linear.V2 (V2 (..))
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P hiding (space)

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

lexemeFull :: Parser a -> Parser a
lexemeFull = P.lexeme P.space

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

signedNumberP :: Parser Int
signedNumberP = P.choice [negate <$ P.char '-', id <$ P.char '+', pure id] <*> numberP

wordP :: Parser Text
wordP = lexeme $ P.takeWhile1P (Just "Word") isAlphaNum

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

greatestCommonDivisor :: (Integral a) => a -> a -> a
greatestCommonDivisor a 0 = abs a
greatestCommonDivisor a b = greatestCommonDivisor b (a `mod` b)

leastCommonMultiple :: (Integral a) => a -> a -> a
leastCommonMultiple a b = abs (a * b) `div` greatestCommonDivisor a b

print2DMap :: M.Map (V2 Int) Char -> IO ()
print2DMap map = do
  let positions = M.keys map
      xs = (\(V2 x _) -> x) <$> positions
      ys = (\(V2 _ y) -> y) <$> positions
      (minX, maxX) = (minimum xs, maximum xs)
      (minY, maxY) = (minimum ys, maximum ys)

  forM_ [minY .. maxY] $ \y -> do
    forM_ [minX .. maxX] $ \x -> do
      putChar $ fromMaybe ' ' $ M.lookup (V2 x y) map
    putStrLn ""

print2DSet :: S.Set (V2 Int) -> IO ()
print2DSet set = print2DMap $ M.fromList ((,'X') <$> S.toList set)

allCombinations :: (Ord a) => S.Set a -> S.Set (a, a)
allCombinations = combinations . S.toList
  where
    combinations [] = S.empty
    combinations (x : xs) = S.fromList (fmap (x,) xs) `S.union` combinations xs

safeHead :: [a] -> Maybe a
safeHead (x : xs) = Just x
safeHead [] = Nothing

mapFirst :: (a -> a) -> [a] -> [a]
mapFirst f (x : xs) = f x : xs
mapFirst _ [] = []

anyCharP :: Parser Char
anyCharP = P.satisfy (>' ')

matrix2DP :: (Char -> a) -> Parser [[a]]
matrix2DP f = many1 $ do
  r <- fmap f <$> many1 anyCharP
  P.newline
  return r

matrix2DVP :: (Char -> a) -> Parser [(V2 Int, a)]
matrix2DVP f = do
  row_major <- matrix2DP f
  pure $
    concatMap
      ( \(rowIndex, row) ->
          ( \(columnIndex, e) ->
              (V2 columnIndex rowIndex, e)
          )
            <$> row
      )
      (zip [0 ..] (zip [0 ..] <$> row_major))

applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes 0 f a = a
applyNTimes x f a = applyNTimes (pred x) f (f a)

replaceList :: Eq a => a -> a -> [a] -> [a]
replaceList r x (y:ys) = if r == y then x : replaceList r x ys else y : replaceList r x ys
replaceList _ _ [] = []

anyChar :: Parser Char
anyChar = P.satisfy (const True)
