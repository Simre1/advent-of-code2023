module Day14.Main where

import Control.Applicative.Combinators qualified as P
import Control.Monad (forM_, when)
import Control.Monad.ST (runST)
import Data.List qualified as L
import Data.List.Split qualified as LS
import Data.Map qualified as M
import Data.Massiv.Array qualified as A
import Data.Massiv.Array.Mutable qualified as A
import Data.Monoid (Sum (..))
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Shared
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P

type Input = MirrorDish

newtype MirrorDish = MirrorDish (A.Array A.U A.Ix2 Char) deriving (Eq, Show, Ord)

data Tilt = TUp | TDown | TLeft | TRight

readInput :: IO Input
readInput = parseFileMega "inputs/day14/input" $ do
  matrix <- matrix2DP id
  pure $ MirrorDish $ A.fromLists' A.Seq matrix

tiltOneStep :: Tilt -> MirrorDish -> MirrorDish
tiltOneStep tilt (MirrorDish dish) = MirrorDish $ runST $ do
  dishM <- A.thawS dish
  forM_ indices $ \index -> do
    startRock <- A.readM dishM index
    when (startRock == 'O') $ do
      destRock <- A.read dishM (index + direction)
      case destRock of
        Just '.' -> A.swapM_ dishM index (index + direction)
        _ -> pure ()
      pure ()
  A.freezeS dishM
  where
    (A.Sz2 nR nC) = A.size dish
    direction = case tilt of
      TUp -> A.Ix2 (-1) 0
      TLeft -> A.Ix2 0 (-1)
      TDown -> A.Ix2 1 0
      TRight -> A.Ix2 0 1
    indices = case tilt of
      TUp -> [A.Ix2 r c | r <- [0 .. nR - 1], c <- [0 .. nC - 1]]
      TLeft -> [A.Ix2 r c | r <- [0 .. nR - 1], c <- [0 .. nC - 1]]
      TDown -> [A.Ix2 r c | r <- [nR - 1, nR - 2 .. 0], c <- [nC - 1, nC - 2 .. 0]]
      TRight -> [A.Ix2 r c | r <- [nR - 1, nR - 2 .. 0], c <- [nC - 1, nC - 2 .. 0]]

tiltFully :: Tilt -> MirrorDish -> MirrorDish
tiltFully tilt md =
  let tilted = tiltOneStep tilt md
   in if md == tilted then md else tiltFully tilt tilted

cycleMirrorDish :: MirrorDish -> MirrorDish
cycleMirrorDish md = tiltFully TRight $ tiltFully TDown $ tiltFully TLeft $ tiltFully TUp md

findCycleLoop :: MirrorDish -> (MirrorDish, Int, Int)
findCycleLoop = go M.empty 0
  where
    go visited step md = case M.lookup md visited of
      Just previousStep -> (md, previousStep, step)
      Nothing -> go (M.insert md step visited) (succ step) $ cycleMirrorDish md

computeWeight :: MirrorDish -> Int
computeWeight (MirrorDish dish) =
  getSum $
    A.ifoldMono (\(A.Ix2 r _) rock -> if rock == 'O' then Sum (nR - r) else Sum 0) dish
  where
    (A.Sz2 nR _) = A.size dish

solution1 :: Input -> IO ()
solution1 input =
  print $ computeWeight $ tiltFully TUp input

solution2 :: Input -> IO ()
solution2 input = 
  let (fixPointMirrorDish, startCycle, endCycle) = findCycleLoop input
      remainingCycles = (simulationCycles - startCycle) `mod` (endCycle - startCycle)
      finalMirrorDish = applyNTimes remainingCycles cycleMirrorDish fixPointMirrorDish
  in print $ computeWeight finalMirrorDish
  where 
    simulationCycles = 1000000000
  
main :: IO ()
main = do
  input <- readInput
  solution1 input
  solution2 input
