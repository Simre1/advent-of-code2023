module Day23.Main where

import Algorithm.Search (aStar, dijkstra)
import Control.Applicative.Combinators qualified as P
import Control.Parallel.Strategies
import Data.List qualified as L
import Data.List.Split qualified as LS
import Data.Map qualified as M
import Data.Massiv.Array qualified as A
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Debug.Trace
import GHC.Conc (par)
import Shared
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P
import Data.Bifunctor (Bifunctor(second))

type Input = HikingMap

newtype HikingMap = HikingMap (A.Array A.B A.Ix2 Path) deriving (Eq, Show, Ord)

data Path = Open | Forest | Slope Direction deriving (Eq, Show, Ord)

newtype HikingGraph = HikingGraph (M.Map A.Ix2 [(A.Ix2, Int)]) deriving (Eq, Show, Ord)

readInput :: IO Input
readInput = parseFileMega "inputs/day23/input" $ do
  hikingMap <- matrix2DP $ \case
    '.' -> Open
    '#' -> Forest
    '>' -> Slope DRight
    '<' -> Slope DLeft
    'v' -> Slope DDown
    '^' -> Slope DUp
    _ -> error "Not a valid path symbol"
  pure $ HikingMap $ A.fromLists' A.Seq hikingMap

makeHikingGraph :: Bool -> A.Ix2 -> A.Ix2 -> HikingMap -> HikingGraph
makeHikingGraph slopePassable start end (HikingMap hikingMap) = HikingGraph $ M.fromList $ zip vertices $ findEdges 0 S.empty <$> vertices
  where
    (A.Sz2 nR nC) = A.size hikingMap
    vertices = filter isVertex [A.Ix2 r c | r <- [0 .. nR - 1], c <- [0 .. nC - 1]]
    isPassable pos = case hikingMap A.!? pos of
      Nothing -> False
      Just Forest -> False
      _ -> True
    isVertex pos
      | pos == start || pos == end = True
      | otherwise =
          let horizontal = isPassable (direction DLeft + pos) && isPassable (direction DRight + pos)
              vertical = isPassable (direction DUp + pos) && isPassable (direction DDown + pos)
           in case hikingMap A.!? pos of
                Just Forest -> False
                Just Open -> length (filter (isPassable . (pos +) . direction) [minBound .. maxBound]) >= 3
                Just (Slope DRight) -> False
                Just (Slope DLeft) -> False
                Just (Slope DDown) -> False
                Just (Slope DUp) -> False
                Nothing -> False
    findEdges :: Int -> S.Set A.Ix2 -> A.Ix2 -> [(A.Ix2, Int)]
    findEdges !pathLength !visited !current
      | S.member current visited = []
      | isVertex current && pathLength /= 0 = [(current, pathLength)]
      | otherwise =
          let nextVisited = S.insert current visited
           in case hikingMap A.!? current of
                Nothing -> []
                Just Forest -> []
                Just Open -> mconcat $ findEdges (succ pathLength) nextVisited . (+ current) . direction <$> [minBound .. maxBound]
                Just (Slope d) ->
                  let d1 = findEdges (succ pathLength) nextVisited (current + direction d)
                      d2 = findEdges (succ pathLength) nextVisited (current - direction d)
                   in if slopePassable then mconcat [d1, d2] else d1

findLongestPath :: HikingGraph -> A.Ix2 -> A.Ix2 -> Int
findLongestPath (HikingGraph hikingGraph) start end = go S.empty 0 start
  where
    go !visited !pathLength !current
      | S.member current visited = 0
      | current == end = pathLength
      | otherwise =
          let nextVertices = hikingGraph M.! current
              nextVisited = S.insert current visited
           in maximum $ uncurry (flip $ go nextVisited) <$> fmap (second (+pathLength)) nextVertices

direction DRight = A.Ix2 0 1
direction DLeft = A.Ix2 0 (-1)
direction DDown = A.Ix2 1 0
direction DUp = A.Ix2 (-1) 0

startAndEnd :: HikingMap -> (A.Ix2, A.Ix2)
startAndEnd (HikingMap hikingMap) =
  let (A.Sz2 nR nC) = A.size hikingMap
   in (A.Ix2 0 1, A.Ix2 (nR - 1) (nC - 2))

solution1 :: Input -> IO ()
solution1 input = do
  let (start, end) = startAndEnd input
      hikingGraph = makeHikingGraph False start end input
  print $ findLongestPath hikingGraph start end

solution2 :: Input -> IO ()
solution2 input = do
  let (start, end) = startAndEnd input
      hikingGraph = makeHikingGraph True start end input
  print $ findLongestPath hikingGraph start end

main :: IO ()
main = do
  input <- readInput
  solution1 input
  solution2 input
