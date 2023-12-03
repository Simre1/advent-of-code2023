module Day3.Main where

import Control.Applicative.Combinators qualified as P
import Data.Char (isNumber)
import Data.List qualified as L
import Data.List.Split qualified as LS
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Vector qualified as V
import Linear.V2
import Shared
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P

data PartNumber = PartNumber Int (S.Set (V2 Int)) deriving (Show)

type Input = (M.Map (V2 Int) Char, [PartNumber])

readInput :: IO Input
readInput = do
  grid <- gridParse <$> readFile "inputs/day3/input1"
  let parts = M.fromList $ filter (\(_, e) -> not $ isNumber e || e == '.') grid
      numbers = catMaybes $ snd $ L.mapAccumL sumPartNumbers (V2 0 0, "") grid
  pure (parts, numbers)
  where
    sumPartNumbers (lastPosition, lastNumbers) (currentPosition, element)
      | not (isNumber element) || lastPosition + V2 1 0 /= currentPosition =
          ( (currentPosition, if isNumber element then [element] else ""),
            if not (null lastNumbers)
              then Just (PartNumber (read lastNumbers) (S.fromList $ (\x -> lastPosition - V2 x 0) <$> [0 .. length lastNumbers - 1]))
              else Nothing
          )
      | otherwise = ((currentPosition, lastNumbers ++ [element]), Nothing)

solution1 :: Input -> IO ()
solution1 (partPositions, partNumbers) =
  print $
    sum ((\(PartNumber number _) -> number) <$> filter filterPartNumber partNumbers)
  where
    filterPartNumber (PartNumber _ numberPosition) =
      let possiblePartPositions = S.unions $ surroundings `S.map` numberPosition
       in not $ S.null $ possiblePartPositions `S.intersection` M.keysSet partPositions

surroundings (V2 x y) = S.fromList $ [V2 a b | a <- [x - 1, x, x + 1], b <- [y - 1, y, y + 1]]

solution2 :: Input -> IO ()
solution2 (partPositions, partNumbers) =
  print $ sum $ M.map (\partNumbers -> if length partNumbers > 1 then product partNumbers else 0) $ 
    L.foldl'
      ( \gearRatios (PartNumber number partPositions) ->
          L.foldl' (flip (M.alter (fmap (number:)))) gearRatios $ S.unions $ surroundings `S.map` partPositions
      )
      initialGearRatios
      partNumbers
  where
    gears = M.filter (== '*') partPositions
    initialGearRatios = fmap (const []) gears

main :: IO ()
main = do
  input <- readInput
  solution1 input
  solution2 input
