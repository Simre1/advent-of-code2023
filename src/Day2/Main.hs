module Day2.Main where

import Control.Applicative (Alternative (..))
import Control.Applicative.Combinators qualified as P
import Data.Char (isNumber)
import Data.List qualified as L
import Data.List.Split qualified as LS
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Vector qualified as V
import Debug.Trace
import Shared
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P

data Game = Game Int [Round] deriving (Show)

data Round = Round
  { red :: Int,
    green :: Int,
    blue :: Int
  }
  deriving (Show)

type Input = [Game]

readInput :: IO Input
readInput = parseFileMega "inputs/day2/input1" $ parseLines $ P.try $ do
  lexeme $ P.string "Game"
  gameId <- numberP
  lexeme $ P.char ':'
  fmap (Game gameId) $ many1 $ P.try $ do
    r <- accumulateP1 (Round 0 0 0) colorP
    P.optional $ lexeme $ P.char ';'
    pure r
  where
    colorP = P.try $ lexeme $ do
      amount <- numberP
      fn <-
        lexeme $
          P.choice
            [ (\r -> r {blue = amount}) <$ P.string "blue",
              (\r -> r {red = amount}) <$ P.string "red",
              (\r -> r {green = amount}) <$ P.string "green"
            ]
      lexeme $ P.optional $ P.char ','
      pure fn

knownGameDice :: Game -> (Int, Int, Int)
knownGameDice (Game _ rounds) =
  L.foldl' (\(maxR, maxG, maxB) (Round red green blue) -> (max maxR red, max maxG green, max maxB blue)) (0, 0, 0) rounds

gamePossible :: (Int, Int, Int) -> Game -> Bool
gamePossible (maxR, maxG, maxB) game =
  let (gameR, gameG, gameB) = knownGameDice game
  in maxR >= gameR && maxG >= gameG && maxB >= gameB

solution1 :: Input -> IO ()
solution1 input =
  print $ sum ((\(Game gameId _) -> gameId) <$> filter (gamePossible (12, 13, 14)) input)

solution2 :: Input -> IO ()
solution2 input = 
  print $ sum $ (\(r,g,b) -> r * g * b) . knownGameDice <$> input

main :: IO ()
main = do
  input <- readInput
  solution1 input
  solution2 input
