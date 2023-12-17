module Day17.Main where

import Algorithm.Search (aStar)
import Control.Applicative.Combinators qualified as P
import Data.Bifunctor (Bifunctor (..))
import Data.Functor ((<&>))
import Data.List qualified as L
import Data.List.Split qualified as LS
import Data.Map qualified as M
import Data.Massiv.Array qualified as A
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Shared
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P

type Input = City

newtype City = City (A.Array A.U A.Ix2 Int) deriving (Eq, Show, Ord)

cityTopLeft :: City -> A.Ix2
cityTopLeft (City city) = A.Ix2 0 0

cityBottomRight :: City -> A.Ix2
cityBottomRight (City city) =
  let (A.Sz2 r c) = A.size city
   in A.Ix2 (pred r) (pred c)

readInput :: IO Input
readInput =
  parseFileMega "inputs/day17/input" $
    City . A.fromLists' A.Seq <$> matrix2DP (read . pure)

findBestPath :: (A.Ix2 -> Int -> A.Ix2 -> Bool) -> ((A.Ix2, Int, A.Ix2) -> Int) -> ((A.Ix2, Int, A.Ix2) -> Bool) -> A.Ix2 -> A.Ix2 -> City -> Maybe (Int, [A.Ix2])
findBestPath turningCondition heuristic isGoal startPosition endPosition (City city) = second (fmap (\(p, _, _) -> p)) <$> aStar nextStates costs heuristic isGoal initial
  where
    nextStates (position, straightMoves, lastDirection) =
      let directions = [A.Ix2 1 0, A.Ix2 (-1) 0, A.Ix2 0 (-1), A.Ix2 0 1]
          filterNonBackwards = filter (/= negate lastDirection)
          validDirections = filter (\d -> turningCondition lastDirection straightMoves d || straightMoves == 0) $ filterNonBackwards directions
       in validDirections <&> \chosenDirection ->
            ( position + chosenDirection,
              if abs chosenDirection == abs lastDirection then succ straightMoves else 1,
              chosenDirection
            )
    costs (_, _, _) (nextPosition, _, _) = fromMaybe 999999 $ city A.!? nextPosition
    initial = (startPosition, 0 :: Int, A.Ix2 0 0)

solution1 :: Input -> IO ()
solution1 input =
  let max3Straight lastDirection straightMoves direction = straightMoves < 3 || abs direction /= abs lastDirection
      endPosition = cityBottomRight input
      max3StraightHeuristic (position, _, _) =
        let (A.Ix2 x y) = abs (endPosition - position)
         in x + y
   in print (fst <$> findBestPath max3Straight max3StraightHeuristic (\(position, _, _) -> position == endPosition) (cityTopLeft input) (cityBottomRight input) input)

solution2 :: Input -> IO ()
solution2 input =
  let min4Max10Straights lastDirection straightMoves direction
        | straightMoves < 4 = abs lastDirection == abs direction
        | straightMoves >= 10 = abs lastDirection /= abs direction
        | otherwise = True

      min4Max10StraightsHeuristic (position, straightMoves, lastDirection) =
        let predictedPosition = position + lastDirection * max 0 (4 - fromIntegral straightMoves)
            (A.Ix2 x y) = abs (endPosition - predictedPosition)
         in x + y
      endPosition = cityBottomRight input
      isGoal (position, straightMoves, _) = position == endPosition && straightMoves >= 4
   in print (fst <$> findBestPath min4Max10Straights min4Max10StraightsHeuristic isGoal (cityTopLeft input) (cityBottomRight input) input)

main :: IO ()
main = do
  input <- readInput
  solution1 input
  solution2 input
