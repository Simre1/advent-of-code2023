module Day9.Main where

import Control.Applicative.Combinators qualified as P
import Data.List qualified as L
import Data.List.Split qualified as LS
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Shared
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P

type Input = [Reading]

newtype Reading = Reading [Int] deriving (Eq, Show, Ord)

data GuessAtX = GuessAtX
  { values :: [Int],
    step :: Int
  }
  deriving (Show, Eq, Ord)

reverseReading :: Reading -> Reading
reverseReading (Reading values) = Reading $ reverse values

guessNext :: GuessAtX -> GuessAtX
guessNext (GuessAtX values i) = GuessAtX (go values) (succ i)
  where
    go [x] = [x]
    go (x : xs) = (x + head (go xs)) : go xs

discreteDifferentiate :: Reading -> GuessAtX
discreteDifferentiate (Reading values)
  | all (== 0) values = GuessAtX [] 0
  | otherwise =
      let (GuessAtX nextValues i) = discreteDifferentiate $ Reading $ differences values
       in (GuessAtX (last values : nextValues) $ succ i)

differences [] = []
differences [x] = []
differences (x : y : xs) = (y - x) : differences (y : xs)

readInput :: IO Input
readInput = parseFileMega "inputs/day9/input" $ parseLines $ fmap Reading $ many1 $ lexeme signedNumberP

solution1 :: Input -> IO ()
solution1 input = do
  readings <- readInput
  let guesses = discreteDifferentiate <$> readings
      nextGuesses = guessNext <$> guesses
  print $ sum $ head . (.values) <$> nextGuesses

solution2 :: Input -> IO ()
solution2 input = do
  readings <- fmap reverseReading <$> readInput
  let guesses = discreteDifferentiate <$> readings
      nextGuesses = guessNext <$> guesses
  print $ sum $ head . (.values) <$> nextGuesses

main :: IO ()
main = do
  input <- readInput
  solution1 input
  solution2 input
