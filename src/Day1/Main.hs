module Day1.Main where

import Data.Char (isNumber)
import Data.Functor ((<&>))
import Data.List qualified as L
import Data.List.Split qualified as LS
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Vector qualified as V
import Shared

type Input = [String]

readInput :: IO Input
readInput = lines <$> readFile "inputs/day1/input1"

solution1 :: Input -> IO ()
solution1 input =
  print $
    sum $
      input <&> \line ->
        let numbers = filter isNumber line
         in read [head numbers, last numbers]

findOneOf :: [String] -> String -> String
findOneOf needles str = fromJust $ L.find (`elem` needles) $ L.subsequences str

numbers :: [(String, Char)]
numbers =
  [ ("1", '1'),
    ("2", '2'),
    ("3", '3'),
    ("4", '4'),
    ("5", '5'),
    ("6", '6'),
    ("7", '7'),
    ("8", '8'),
    ("9", '9'),
    ("one", '1'),
    ("two", '2'),
    ("three", '3'),
    ("four", '4'),
    ("five", '5'),
    ("six", '6'),
    ("seven", '7'),
    ("eight", '8'),
    ("nine", '9')
  ]

filterLine = filter (`elem` relevantChars)
  where relevantChars = L.nub $ mconcat $ fst <$> numbers

findFirstNumber line = fromJust (lookup (findOneOf (fst <$> numbers) (filterLine line)) numbers)

findLastNumber line = fromJust (lookup (reverse $ findOneOf (reverse . fst <$> numbers) $ reverse (filterLine line)) numbers)

solution2 :: Input -> IO ()
solution2 input =
  print $
    sum $
      input <&> \line ->
        read [findFirstNumber line, findLastNumber line]

main :: IO ()
main = do
  input <- readInput
  solution1 input
  solution2 input
