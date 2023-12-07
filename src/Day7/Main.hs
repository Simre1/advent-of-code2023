module Day7.Main where

import Control.Applicative.Combinators qualified as P
import Control.Arrow (Arrow (..))
import Data.Char (isLetter)
import Data.Coerce (coerce)
import Data.List qualified as L
import Data.List.Split qualified as LS
import Data.Map qualified as M
import Data.Ord (Down (..))
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Debug.Trace (traceShow)
import Shared
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P

type Input = [(Hand, Bid)]

newtype Bid = Bid Int deriving (Eq, Show, Ord)

newtype Hand = Hand [Char] deriving (Eq, Show)

instance Ord Hand where
  h1 `compare` h2 = orderHand h1 h2

cardStrengths :: M.Map Char Int
cardStrengths =
  M.fromList
    [('A', 13), ('K', 12), ('Q', 11), ('J', 10), ('T', 9), ('9', 8), ('8', 7), ('7', 6), ('6', 5), ('5', 4), ('4', 3), ('3', 2), ('2', 1), ('O', 0)]

jToJoker :: Input -> Input
jToJoker = fmap $ first (\(Hand cards) -> Hand $ fmap (\c -> if c == 'J' then 'O' else c) cards)

readInput :: IO Input
readInput = parseFileMega "inputs/day7/input" $ parseLines $ do
  cards <- lexeme $ P.takeWhile1P Nothing (`M.member` cardStrengths)
  bid <- lexeme numberP
  pure
    ( Hand $
        T.unpack cards,
      Bid bid
    )

groupHand :: Hand -> [(Char, Int)]
groupHand (Hand cards) =
  if jokers == 5
    then [('O', 5)]
    else
      let (strongestCombo : otherCombos) = groupedNoJokerCards
       in (second (+ jokers) strongestCombo : otherCombos)
  where
    groupedNoJokerCards =
      L.sortBy
        ( \(c1, n1) (c2, n2) -> case n2 `compare` n1 of
            GT -> GT
            LT -> LT
            EQ -> getCardStrength c2 `compare` getCardStrength c1
        )
        ( (\a -> (head a, length a))
            <$> L.group
              ( L.sort
                  noJokerCards
              )
        )
    noJokerCards = filter (/= 'O') cards
    jokers = length cards - length noJokerCards

getCardStrength c = cardStrengths M.! c

orderHand :: Hand -> Hand -> Ordering
orderHand hand1@(Hand cards1) hand2@(Hand cards2) =
  case computeType hand1 `compare` computeType hand2 of
    GT -> GT
    LT -> LT
    EQ -> fmap getCardStrength cards1 `compare` fmap getCardStrength cards2

computeType :: Hand -> Int
computeType hand = case groupHand hand of
  (card, 5) : xs -> 6
  (card, 4) : xs -> 5
  (card3, 3) : (card2, 2) : xs -> 4
  (card, 3) : xs -> 3
  (card3, 2) : (card2, 2) : xs -> 2
  (card, 2) : xs -> 1
  (card, 1) : xs -> 0

solution1 :: Input -> IO ()
solution1 input = do
  print $ sum $ zipWith (*) [(1 :: Int) ..] (coerce . snd <$> L.sortOn fst input)

solution2 :: Input -> IO ()
solution2 input = do
  print $ sum $ zipWith (*) [(1 :: Int) ..] (coerce . snd <$> L.sortOn fst (jToJoker input))

main :: IO ()
main = do
  input <- readInput
  solution1 input
  solution2 input
