{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <$>" #-}
module Day10.Main where

import Algorithm.Search (dfs, dfsM, dijkstra, pruning)
import Control.Applicative.Combinators qualified as P
import Control.Monad (unless, when)
import Control.Monad.Trans.State
import Data.List qualified as L
import Data.List.Split qualified as LS
import Data.Map qualified as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Debug.Trace (traceShow, traceShowId)
import Linear.V2
import Shared
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P

type Input = M.Map (V2 Int) Char

readInput :: IO Input
readInput = do
  file <- readFile "inputs/day10/input"
  let grid = M.fromList $ gridParse file
  pure grid

findStart :: Input -> V2 Int
findStart input = fst $ M.elemAt 0 (M.filter (== 'S') input)

directions :: Input -> V2 Int -> [V2 Int]
directions input position = case input M.! position of
  '|' -> addPosition [V2 0 1, V2 0 (-1)]
  '-' -> addPosition [V2 1 0, V2 (-1) 0]
  'L' -> addPosition [V2 1 0, V2 0 (-1)]
  'J' -> addPosition [V2 (-1) 0, V2 0 (-1)]
  '7' -> addPosition [V2 (-1) 0, V2 0 1]
  'F' -> addPosition [V2 1 0, V2 0 1]
  '.' -> []
  'S' ->
    let possibleMoves = addPosition [V2 1 0, V2 (-1) 0, V2 0 1, V2 0 (-1)]
     in fmap snd $
          filter (\(backMoves, move) -> position `elem` backMoves) $
            zip (directions input <$> possibleMoves) possibleMoves
  where
    addPosition moves = filter (`M.member` input) $ (position +) <$> moves

findLoop :: Input -> [V2 Int]
findLoop input =
  (startPosition :) $ fmap fst $ fromMaybe [] $ evalState (dfsM nextStates isEnd (startPosition, V2 0 0)) S.empty
  where
    nextStates (position, incomingDirection) = do
      visited <- get
      when (position /= startPosition) $
        modify $
          S.insert position
      let possibleNextPositions = filter (not . (`S.member` visited)) (directions input position)
      let noBackwardPositions =
            filter (\(nextPosition, nextDirection) -> nextDirection /= negate incomingDirection) $
              fmap (\nextPosition -> (nextPosition, nextPosition - position)) possibleNextPositions
      pure noBackwardPositions
    isEnd (position, incomingDirection) = do
      pure $ startPosition == position && (incomingDirection /= V2 0 0)
    startPosition = findStart input

traceLoop :: [V2 Int] -> [V2 Int]
traceLoop [] = []
traceLoop [x] = []
traceLoop (x : y : xs) = (y - x) : traceLoop (y : xs)

detectEdges :: [V2 Int] -> [V2 Int]
detectEdges positions =
  let trace = traceLoop positions
      trace' = last trace : trace
  in go trace'
  where
    go [] = []
    go [x] = []
    go (x:y:xs) = fmap (`quot` 2) (2 * x + 2 * y) : go (y : xs)

edgeMap :: [V2 Int] -> M.Map (V2 Int) (V2 Int)
edgeMap loop = M.fromList $ zip loop (detectEdges loop)

findEnclosedSpaces :: M.Map (V2 Int) (V2 Int) -> S.Set (V2 Int) -> S.Set (V2 Int)
findEnclosedSpaces loop = S.filter surroundedByLoop
  where

    surroundedByLoop (V2 x y) =
      let (V2 leftX leftY) = sum $ M.filterWithKey (\(V2 lX lY) _ -> lY == y && lX < x) loop
          (V2 rightX rightY) = sum $ M.filterWithKey (\(V2 lX lY) _ -> lY == y && lX > x) loop
          (V2 topX topY) = sum $ M.filterWithKey (\(V2 lX lY) _ -> lX == x && lY < y) loop
          (V2 bottomX bottomY) = sum $ M.filterWithKey (\(V2 lX lY) _ -> lX == x && lY > y) loop
      in abs leftY > 0 && abs rightY > 0 && abs topX > 0 && abs bottomX > 0

potentialSpaces :: Input -> [V2 Int] -> S.Set (V2 Int)
potentialSpaces input loop = M.keysSet input `S.difference` S.fromList loop

solution1 :: Input -> IO ()
solution1 input = print $ (length (findLoop input) - 1) `quot` 2

solution2 :: Input -> IO ()
solution2 input = do
  let loop = findLoop input
      edges = edgeMap loop
  print $ length $ findEnclosedSpaces edges $ potentialSpaces input loop

main :: IO ()
main = do
  input <- readInput
  solution1 input
  solution2 input
