module Day13.Main where

import Control.Applicative ((<|>))
import Control.Applicative.Combinators qualified as P
import Data.List qualified as L
import Data.List.Split qualified as LS
import Data.Map qualified as M
import Data.Massiv.Array qualified as A
import Data.Maybe (fromJust)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Traversable (for)
import Data.Vector qualified as V
import Linear.V2
import Shared
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P

data Terrain = A | R deriving (Eq, Ord, Show, Enum)

newtype Pattern = Pattern (A.Array A.B A.Ix2 Terrain) deriving (Eq, Show)

type Matrix = A.Array A.B A.Ix2

type Input = [Pattern]

readInput :: IO Input
readInput = do
  parseFileMega "inputs/day13/input" $
    many1 $
      fmap (Pattern . A.fromLists' A.Seq) $
        lexemeFull $
          matrix2DP (\c -> if c == '#' then R else A)

matchRegions :: Pattern -> M.Map (V2 Int) (Matrix Terrain, Matrix Terrain)
matchRegions (Pattern matrix) =
  let rowMirrors = M.fromList $ flip fmap [1 .. nR - 1] $ \row ->
        let reflectSizeY = min row (nR - row)
            top = A.compute $ A.extract' (A.Ix2 (row - reflectSizeY) 0) (A.Sz2 reflectSizeY nC) matrix
            bottom = A.compute $ A.reverse A.Dim2 $ A.extract' (A.Ix2 row 0) (A.Sz2 reflectSizeY nC) matrix
         in (V2 0 row, (top, bottom))

      columnMirrors = M.fromList $ flip fmap [1 .. nC - 1] $ \col ->
        let reflectSizeX = min col (nC - col)
            left = A.compute $ A.extract' (A.Ix2 0 (col - reflectSizeX)) (A.Sz2 nR reflectSizeX) matrix
            right = A.compute $ A.reverse A.Dim1 $ A.extract' (A.Ix2 0 col) (A.Sz2 nR reflectSizeX) matrix
         in (V2 col 0, (left, right))
   in rowMirrors <> columnMirrors
  where
    (A.Sz2 nR nC) = A.size matrix

findMirrors :: M.Map (V2 Int) (Matrix Terrain, Matrix Terrain) -> S.Set (V2 Int)
findMirrors = M.keysSet . M.filter (uncurry (==))

findSmudge :: V2 Int -> Matrix Terrain -> Matrix Terrain -> Maybe A.Ix2
findSmudge (V2 c r) a b =
  let differences = abs <$> A.zipWith (-) (fmap fromEnum a) (fmap fromEnum b)
   in if sum differences == 1
        then
          Just $
            let topLeftIx = A.ifoldlS (\a ix e -> if e == 1 then ix else a) (A.Ix2 (-1) (-1)) differences
             in (topLeftIx + A.Ix2 (max 0 $ r - nR) (max 0 $ c - nC))
        else Nothing
  where
    (A.Sz2 nR nC) = A.size a

flipSpot :: A.Ix2 -> Pattern -> Pattern
flipSpot ix (Pattern matrix) = Pattern $ A.compute $ A.imap (\ix' e -> if ix' == ix then if e == A then R else A else e) matrix

solution1 :: Input -> IO ()
solution1 input =
  print $ sum $ sum $ sum . S.map (V2 1 100 *) . findMirrors . matchRegions <$> input

solution2 :: Input -> IO ()
solution2 input = do
  let regions = matchRegions <$> input
      smudges = (fromJust . L.foldl' (<|>) Nothing . fmap (\(reflectionLine, (m1, m2)) -> findSmudge reflectionLine m1 m2)) . M.toList <$> regions
      oldLines = findMirrors . matchRegions <$> input
      fixedInput = zipWith flipSpot smudges input
      newLines = findMirrors . matchRegions <$> fixedInput
  print $ sum $ sum $ sum . S.map (V2 1 100 *) <$> zipWith S.difference newLines oldLines

main :: IO ()
main = do
  input <- readInput
  solution1 input
  solution2 input
