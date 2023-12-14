module Day12.Main where

import Control.Applicative ((<|>))
import Data.Bifunctor (Bifunctor (..))
import Data.List qualified as L
import Data.List.Split qualified as LS
import Data.Map qualified as M
import Data.MemoTrie (HasTrie (..), memo2)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Debug.Trace (traceShow, traceShowId)
import GHC.Generics (Generic, Rep, from, to)
import Shared
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P

type Input = [SpringRow]

data SpringRow = SpringRow [SpringState] [Int] deriving (Show, Eq, Ord)

data SpringState = Working | Broken | Unknown deriving (Show, Eq, Ord, Generic, Enum)

instance HasTrie SpringState where
  data SpringState :->: x = SpringStateTrie x x x
  trie f = SpringStateTrie (f Working) (f Broken) (f Unknown)
  untrie (SpringStateTrie a b c) x
    | x == Working = a
    | x == Broken = b
    | otherwise = c
  enumerate (SpringStateTrie a b c) = [(Working, a), (Broken, b), (Unknown, c)]

readInput :: IO Input
readInput = parseFileMega "inputs/day12/input" $ parseLines $ do
  springStates <- lexeme $ many1 $ Broken <$ P.char '#' <|> Working <$ P.char '.' <|> Unknown <$ P.char '?'
  groups <- lexeme numberP `P.sepBy` P.char ','
  pure $ SpringRow springStates groups

unfoldInput :: Int -> Input -> Input
unfoldInput multiplier = fmap $ \(SpringRow states groups) ->
  SpringRow (L.intercalate [Unknown] (replicate multiplier states)) (concat $ replicate multiplier groups)

countCombinations :: SpringRow -> Int
countCombinations (SpringRow states groups) = go states groups
  where
    go = memo2 go'
    go' (Working : xs) gs = go xs gs
    go' (Broken : xs) gs = goBroken (Broken : xs) gs
    go' (Unknown : xs) gs = go (Working : xs) gs + goBroken (Broken : xs) gs
    go' [] [] = 1
    go' [] (g : gs) = 0

    goBroken (Broken : xs) gs = goBroken xs $ mapFirst pred gs
    goBroken (Working : xs) (0 : gs) = go (Working : xs) gs
    goBroken (Working : xs) (g : gs) = 0
    goBroken (Unknown : xs) gs = goBroken (Working : xs) gs + goBroken (Broken : xs) gs
    goBroken [] [0] = 1
    goBroken [] _ = 0
    goBroken xs [] = 0

solution1 :: Input -> IO ()
solution1 input = print $ sum (countCombinations <$> input)

solution2 :: Input -> IO ()
solution2 input = do
  print $ sum (countCombinations <$> unfoldInput 5 input)
-- print $ tripleTry

main :: IO ()
main = do
  input <- readInput
  solution1 input
  solution2 input
