module Day4.Main where

import Data.List qualified as L
import Data.List.Split qualified as LS
import Data.Map qualified as M
import Data.Monoid
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Vector qualified as V
import Debug.Trace (traceShowId)
import Shared
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P

data Card = Card
  { cardId :: Int,
    winning :: S.Set Int,
    own :: S.Set Int
  }
  deriving (Show)

type Input = [Card]

readInput :: IO Input
readInput = parseFileMega "inputs/day4/input1" $ parseLines $ do
  lexeme $ P.string "Card"
  cardId <- lexeme numberP
  lexeme $ P.string ":"
  winningNumbers <- P.manyTill (lexeme numberP) (lexeme $ P.char '|')
  ownNumbers <- many1 (lexeme numberP)
  pure $ Card cardId (S.fromList winningNumbers) (S.fromList ownNumbers)

solution1 :: Input -> IO ()
solution1 input = print $ flip foldMap input $ \card ->
  Sum $
    let numberOfWinning = winningNumbers card
     in if numberOfWinning > 0
          then 2 ^ (numberOfWinning - 1)
          else 0

winningNumbers :: Card -> Int
winningNumbers (Card id winning own) = S.size (S.intersection winning own)

solution2 :: Input -> IO ()
solution2 input = do
  let cards = L.foldr accumulateCards M.empty input
  print $ sum cards
  where
    accumulateCards :: Card -> M.Map Int Int -> M.Map Int Int
    accumulateCards card@(Card id _ _) cardMap =
      let prizeCards = sum $ (cardMap M.!) <$> [id + 1 .. id + winningNumbers card]
          currentCard = 1
       in M.insert id (prizeCards + currentCard) cardMap

main :: IO ()
main = do
  input <- readInput
  solution1 input
  solution2 input
