module Day16.Main where

import Control.Applicative.Combinators qualified as P
import Data.Bifunctor (Bifunctor (..))
import Data.List qualified as L
import Data.List.Split qualified as LS
import Data.Map qualified as M
import Data.Massiv.Array qualified as A
import Data.Maybe (maybeToList)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Debug.Trace (traceShow)
import Linear (V2 (..))
import Shared
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P

type Input = LightGrid

newtype LightGrid = LightGrid (A.Array A.U A.Ix2 Char) deriving (Eq, Show, Ord)

readInput :: IO Input
readInput =
  parseFileMega "inputs/day16/input" $
    LightGrid . A.fromLists' A.Seq <$> matrix2DP id

data Direction = DUp | DDown | DLeft | DRight deriving (Eq, Show, Ord)

toIndexDelta :: Direction -> (A.Ix2, Direction)
toIndexDelta direction = (,direction) $ case direction of
  DUp -> A.Ix2 (-1) 0
  DDown -> A.Ix2 1 0
  DLeft -> A.Ix2 0 (-1)
  DRight -> A.Ix2 0 1

computeNextDirection :: Direction -> Char -> A.Ix2 -> [(A.Ix2, Direction)]
computeNextDirection direction char ix = fmap (first (ix +)) $ case char of
  '.' -> unchangedDirection
  '-' -> if direction `elem` [DUp, DDown] then splittedDirection else unchangedDirection
  '|' -> if direction `elem` [DLeft, DRight] then splittedDirection else unchangedDirection
  '\\' -> case direction of
    DRight -> [toIndexDelta DDown]
    DLeft -> [toIndexDelta DUp]
    DUp -> [toIndexDelta DLeft]
    DDown -> [toIndexDelta DRight]
  '/' -> case direction of
    DRight -> [toIndexDelta DUp]
    DLeft -> [toIndexDelta DDown]
    DUp -> [toIndexDelta DRight]
    DDown -> [toIndexDelta DLeft]
  _ -> error "Invalid layout"
  where
    unchangedDirection = [toIndexDelta direction]
    splittedDirection =
      let both = fmap toIndexDelta
       in case direction of
            DUp -> both [DRight, DLeft]
            DDown -> both [DRight, DLeft]
            DLeft -> both [DUp, DDown]
            DRight -> both [DUp, DDown]

follow :: LightGrid -> A.Ix2 -> Direction -> S.Set A.Ix2
follow (LightGrid grid) start startDirection = S.map fst $ go S.empty start startDirection
  where
    go visited position direction
      | (position, direction) `S.member` visited = visited
      | otherwise =
          let nextVisited = S.insert (position, direction) visited
           in case A.index grid position of
                Nothing -> visited
                Just gate ->
                  L.foldl' (\v (nextPosition, nextDirection) -> go v nextPosition nextDirection) nextVisited $
                    computeNextDirection direction gate position

solution1 :: Input -> IO ()
solution1 input =
  print $ length $ follow input (A.Ix2 0 0) DRight

startPositions :: LightGrid -> [(A.Ix2, Direction)]
startPositions (LightGrid grid) =
  mconcat
    [ (\r -> (A.Ix2 r 0, DRight)) <$> [0 .. nR - 1],
      (\r -> (A.Ix2 r (nC - 1), DLeft)) <$> [0 .. nR - 1],
      (\c -> (A.Ix2 0 c, DDown)) <$> [0 .. nC - 1],
      (\c -> (A.Ix2 (nR - 1) c, DUp)) <$> [0 .. nC - 1]
    ]
  where
    (A.Sz2 nR nC) = A.size grid

solution2 :: Input -> IO ()
solution2 input =
  let (bestPosition, bestDirection) =
        head $
          L.sortOn (negate . length . uncurry (follow input)) $
            startPositions input
   in print $ length $ follow input bestPosition bestDirection

main :: IO ()
main = do
  input <- readInput
  solution1 input
  solution2 input
