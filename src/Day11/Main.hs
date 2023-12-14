module Day11.Main where

import Control.Applicative.Combinators qualified as P
import Data.List qualified as L
import Data.List.Split qualified as LS
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Linear.V2
import Shared
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P

type Input = S.Set (V2 Int)

readInput :: IO Input
readInput = do
  file <- readFile "inputs/day11/input"
  let galaxies = filter ((== '#') . snd) $ gridParse file
  pure $ S.fromList $ fst <$> galaxies

expandSpace :: Int -> S.Set (V2 Int) -> S.Set (V2 Int)
expandSpace multiplier = S.fromList . expandY . expandX . S.toList
  where
    expandY :: [V2 Int] -> [V2 Int]
    expandY galaxy =
      let sorted = L.sortOn (\(V2 _ y) -> y) galaxy
          (V2 _ startY) = head sorted
       in goY startY sorted
    goY :: Int -> [V2 Int] -> [V2 Int]
    goY lastY [] = []
    goY lastY ((V2 x y) : vs) =
      let additionalY = (y - lastY - 1) * (multiplier - 1)
       in if additionalY > 0
            then fmap (+ V2 0 additionalY) $ V2 x y : goY y vs
            else V2 x y : goY y vs
    expandX :: [V2 Int] -> [V2 Int]
    expandX galaxy =
      let sorted = L.sortOn (\(V2 x _) -> x) galaxy
          (V2 startX _) = head sorted
       in goX startX sorted
    goX :: Int -> [V2 Int] -> [V2 Int]
    goX lastX [] = []
    goX lastX ((V2 x y) : vs) =
      let additionalX = (x - lastX - 1) * (multiplier - 1)
       in if additionalX > 0
            then fmap (+ V2 additionalX 0) $ V2 x y : goX x vs
            else V2 x y : goX x vs

pathLength :: V2 Int -> V2 Int -> Int
pathLength v1 v2 = sum $ abs $ v2 - v1

solution1 :: Input -> IO ()
solution1 input = do
  print $
    sum $
      fmap (uncurry pathLength) $
        S.toList $
          allCombinations $
            expandSpace 2 input

solution2 :: Input -> IO ()
solution2 input =
  print $
    sum $
      fmap (uncurry pathLength) $
        S.toList $
          allCombinations $
            expandSpace 1000000 input

main :: IO ()
main = do
  input <- readInput
  solution1 input
  solution2 input
