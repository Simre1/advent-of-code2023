module Day18.Main where

import Control.Applicative.Combinators qualified as P
import Data.Bifunctor (Bifunctor (..))
import Data.Either (fromRight)
import Data.List qualified as L
import Data.List.Split qualified as LS
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as T
import Data.Vector qualified as V
import Linear.V2
import Shared
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P

data Instruction = Instruction Char Int Text deriving (Show, Eq, Ord)

type Input = [Instruction]

readInput :: IO Input
readInput = parseFileMega "inputs/day18/example" $ parseLines $ do
  direction <- lexeme anyChar
  amount <- lexeme numberP
  P.string "(#"
  color <- wordP
  P.char ')'
  pure $ Instruction direction amount color

decodeInstruction :: Instruction -> Instruction
decodeInstruction (Instruction _ _ encoded) =
  Instruction direction (fst $ fromRight undefined $ T.hexadecimal $ T.take 5 encoded) ""
  where
    direction = case T.last encoded of
      '0' -> 'R'
      '1' -> 'D'
      '2' -> 'L'
      '3' -> 'U'

vertices :: [Instruction] -> [V2 Int]
vertices = go (V2 0 0)
  where
    go currentPosition [] = []
    go currentPosition (Instruction direction amount _ : is) =
      let nextPosition = currentPosition + directionVector direction * pure amount
       in currentPosition : go nextPosition is
    directionVector 'R' = V2 1 0
    directionVector 'L' = V2 (-1) 0
    directionVector 'U' = V2 0 1
    directionVector 'D' = V2 0 (-1)

perimeterArea :: Input -> Int
perimeterArea = sum . fmap (\(Instruction _ amount _) -> amount)

shoelaceArea :: [V2 Int] -> Int
shoelaceArea p = abs (sum [x0 * y1 - x1 * y0 | (V2 x0 y0, V2 x1 y1) <- segments p]) `quot` 2
  where
    segments p = zip p (tail p ++ [head p])

solution1 :: Input -> IO ()
solution1 input = do
  print $ shoelaceArea (vertices input) + perimeterArea input `quot` 2 + 1

solution2 :: Input -> IO ()
solution2 input' = do
  let input = decodeInstruction <$> input'
  print $ shoelaceArea (vertices input) + perimeterArea input `quot` 2 + 1

main :: IO ()
main = do
  input <- readInput
  solution1 input
  solution2 input
