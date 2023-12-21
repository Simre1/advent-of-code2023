module Day21.Main where

import Control.Applicative.Combinators qualified as P
import Control.Monad.ST
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
import Debug.Trace
import Shared
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P

data Field = Rock | Reached !Int | GardenPlot deriving (Eq, Show, Ord)

type Input = (A.Ix2, A.Array A.B A.Ix2 Field)

readInput :: IO Input
readInput = parseFileMega "inputs/day21/input" $ do
  position <- P.lookAhead $ do
    P.takeWhileP (Just "FindStart") (/= 'S')
    position <- P.getSourcePos
    pure $ A.Ix2 (P.unPos (P.sourceLine position) - 1) (P.unPos (P.sourceColumn position) - 1)

  matrix <- fmap (A.fromLists' A.Seq) $ matrix2DP $ \case
    '#' -> Rock
    '.' -> GardenPlot
    'S' -> GardenPlot

  pure (position, matrix)

determineReachability :: Int -> A.Ix2 -> A.Array A.B A.Ix2 Field -> A.Array A.B A.Ix2 Field
determineReachability steps position initialArray = runST $ do
  initialMap <- A.thawS initialArray
  newMap <- goAll steps position initialMap >>= keepOnlyReachable
  A.freezeS newMap
  where
    keepOnlyReachable :: A.MArray s A.B A.Ix2 Field -> ST s (A.MArray s A.B A.Ix2 Field)
    keepOnlyReachable map = do
      A.forPrimM map $ \case
        Reached step ->
          if even step
            then pure $ Reached step
            else pure GardenPlot
        e -> pure e
      pure map

    goAll :: Int -> A.Ix2 -> A.MArray s A.B A.Ix2 Field -> ST s (A.MArray s A.B A.Ix2 Field)
    goAll (-1) position map = pure map
    goAll steps position map = do
      let goNext = L.foldl' (\m direction -> m >>= goAll (steps - 1) (position + direction)) (pure map) directions
      currentField <- A.read map position
      case currentField of
        Just Rock -> pure map
        Just (Reached oldSteps) ->
          if steps > oldSteps
            then do
              A.write map position (Reached steps)
              goNext
            else pure map
        Just GardenPlot -> do
          A.write map position (Reached steps)
          goNext
        Nothing -> pure map

    directions = [A.Ix2 1 0, A.Ix2 (-1) 0, A.Ix2 0 1, A.Ix2 0 (-1)]

countReachable :: A.Array A.B A.Ix2 Field -> Int
countReachable map = getSum $ flip foldMap map $ \case
  Reached _ -> Sum 1
  _ -> Sum 0

increaseMap :: Int -> A.Array A.B A.Ix2 a -> A.Array A.B A.Ix2 a
increaseMap n' arr =
  let n = n' * 2 + 1
      column = A.compute @A.B $ A.concat' (A.Dim 1) $ replicate n arr
   in A.compute $ A.concat' (A.Dim 2) $ replicate n column

-- THX to https://topaz.github.io/paste/#XQAAAQBGCwAAAAAAAAARiEJGPfQWNG6xo4rUnrU/FzUtu7JQVAYt38Ln+Ak4cSYj1a6lO5Ql5zsGtTUzJX5bwpIQ0QTX8vUccaLg8K/uMMUq1ao9PNeqR6zomT1vl1iiE5gfl6FpYmtdaoEXFDuV6M53MR7ypNlFRxk1feNQkx/D8pMiOPkshJ6TUKAbRMGSmfDDdzcLHoHzVsp4OxtidEMN+QGnC7+eirB5pphVDu/dncYe7S7npSbB6ccnQSwXCbsUt8catCF7cxUD2sFo9ZmegNIgzOhxeIHbN/xbVsYlpthYue+PymXOQmF50aVb0MjmI1ICMB2QgxXvxnfSKWvR7Sht1/nmeYj3kcWShAdNZu0RxDQp9O0aqePG0RZrzeC+IXk52q/h8MbfX9GMrq2uawPGyLY1Lvtb/BdOJKLqwDhiZMQAbJ5h6ehDELFSE1cQnsscJGggKYNoRtBDnvM6pW3KDvtfqoLQEs4GyBvHvA6hz/J91WaHxMsRG3o0NWvGGdV0IjqVKsl31VVacK1jraek+D8Ki3xsZ8ZrDHpzwX79ZKCqeUmqoZWCT3o/yZeirESiodu8o094894wX+ty+5KnerPlZsBSM3JV2ZNwavBGJggxVDVzR9WLK3Ik5N4KNcX2aHsX0FWtNEjqYNwq0KVqHz3A6P/f0L7JImqAzz7KuRUCyih57rvbMCR7KMUbYW6Fs4ixP1rm5AFqOU5Ko6a7ocuf5N6G3Fy2iysPrE2tPexXF7xjEKhbaGf2Woeb4/D20MGzY8izj7599tzErQ69SW+QtLWIgXuCrZ+rBUL2iNzTIeN4vcd8nlkmoCZbQyja4epn4cDyAve41Wr4MhjMQFKvH9qHUs8h9c4ZLDXM/KEe0h+cEEKGgDFOjHBRADEdfy878XnZJ6CBqs/mbiniRdzFrPIDZlv2R1mW68p03i00JUMLot+dFrPkjALFMMrnIECVVwgeI8GXF6Qv4nTs/Fai4UFaTA+6941VdgNYqHYC/cpFjMiuKY42GEZMgu8aGaNEPlSOyR3njewsvF2oZCtLm4//9AQDYXDsDsgBsEu3LLw5HrWWs7tu8ir5Fe75AnMOchruN7L+HocDr1n0ADeweHE96mq3R2ABbhKK8dtq5ryd4JfwglTFbKWh9ht/hNERTa+iNx2tlxBpMFyrcpZ4NB9xK5nZLLRfYL2kWiORye1XJSmZZ5UrD8ifbQNHa9fmyTCEPiF0AgevVo8IoMf+AWsbMA7sHQ4yUcUSGvpHF1X4Lbu4mXORBP76PCfm
computePart2 :: [Int] -> Int
computePart2 coefficients =
  let m = coefficients !! 1 - coefficients !! 0
      n = coefficients !! 2 - coefficients !! 1
      a = (n - m) `quot` 2
      b = m - 3 * a
      c = coefficients !! 0 - b - a
      ceiled = ceiling (26501365 / 131)
   in a * ceiled ^ 2 + b * ceiled + c

solution1 :: Input -> IO ()
solution1 (position, map) = print $ countReachable $ determineReachability 64 position map

solution2 :: Input -> IO ()
solution2 (position, map) = do
  let is = [0 .. 2]
      biggerMap = increaseMap 2 map
      coefficients = count biggerMap <$> is
  print $ computePart2 coefficients
  where
    count m i =
      let step = 131 * i + 65
          start = 131 * i + 65
       in countReachable $ determineReachability step (A.Ix2 start start) m
    (A.Sz2 nR nC) = A.size map

main :: IO ()
main = do
  input <- readInput
  solution1 input
  solution2 input
