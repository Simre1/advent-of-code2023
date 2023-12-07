module Day6.Main where

import Control.Applicative.Combinators qualified as P
import Data.Bifunctor (Bifunctor (..))
import Data.List qualified as L
import Data.List.Split qualified as LS
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Shared
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P

data Race = Race
  { time :: Int,
    distance :: Int
  }
  deriving (Eq, Show, Ord)

type Input = [Race]

readInput :: IO Input
readInput = parseFileMega "inputs/day6/input" $ do
  lexemeFull $ P.string "Time:"
  times <- many1 $ lexemeFull numberP
  lexemeFull $ P.string "Distance:"
  distances <- many1 $ lexemeFull numberP
  pure $ zipWith Race times distances

isRaceWin :: Race -> Int -> Bool
isRaceWin (Race time distance) holdTime =
  holdTime * (time - holdTime) > distance

winningHoldTimes :: Race -> [Int]
winningHoldTimes race@(Race time distance) = filter (isRaceWin race) [0 .. time]

solution1 :: Input -> IO ()
solution1 input = print $ product $ length . winningHoldTimes <$> input

solution2 :: Input -> IO ()
solution2 input =
  let (time, distance) =
        bimap read read $
          L.foldl'
            ( \(timeStr, distanceStr) (Race time distance) ->
                (timeStr ++ show time, distanceStr ++ show distance)
            )
            ("", "")
            input
   in print $ length $ winningHoldTimes (Race time distance)

main :: IO ()
main = do
  input <- readInput
  solution1 input
  solution2 input
