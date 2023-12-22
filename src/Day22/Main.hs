module Day22.Main where

import Control.Applicative.Combinators qualified as P hiding (sepBy)
import Data.Functor ((<&>))
import Data.List qualified as L
import Data.List.Split qualified as LS
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Debug.Trace (traceShow, traceShowId)
import Linear (V2 (..))
import Linear.V3
import Optics.Core (lensVL, (^.))
import Shared
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P

type Input = [Brick]

data Brick = Brick Int (V3 Int) (V3 Int) deriving (Eq, Ord, Show)

readInput :: IO Input
readInput = parseFileMega "inputs/day22/input" $ parseLines $ do
  [x1, y1, z1] <- P.sepBy numberP (P.char ',')
  P.char '~'
  [x2, y2, z2] <- P.sepBy numberP (P.char ',')
  sourcePos <- P.getSourcePos
  pure $ Brick (P.unPos $ P.sourceLine sourcePos) (V3 x1 y1 z1) (V3 x2 y2 z2)

brickArea :: Brick -> [V2 Int]
brickArea (Brick _ (V3 sX sY sZ) (V3 eX eY eZ)) =
  let areaX = if sX == eX then [sX] else [sX, sX + signum (eX - sX) .. eX]
      areaY = if sY == eY then [sY] else [sY, sY + signum (eY - sY) .. eY]
   in V2 <$> areaX <*> areaY

simulateFalling :: [Brick] -> S.Set Brick
simulateFalling bricks = S.fromList $ snd $ L.mapAccumL stackBrick M.empty $ sortBricks bricks
  where
    stackBrick floor brick@(Brick brickId (V3 sX sY sZ) (V3 eX eY eZ)) =
      let area = brickArea brick
          floorHeight pos = fromMaybe 0 $ M.lookup pos floor
          heightAfterFalling = succ $ maximum $ floorHeight <$> area
          brickZLength = abs $ sZ - eZ
          -- Assuming that bricks always have a rectangle shape!
          brickAfterFalling = Brick brickId (V3 sX sY heightAfterFalling) (V3 eX eY (heightAfterFalling + brickZLength))
          floorWithFallenBrick = L.foldl' (\f pos -> M.insert pos (heightAfterFalling + brickZLength) f) floor area
       in (floorWithFallenBrick, brickAfterFalling)
    sortBricks = L.sortOn (\(Brick _ posStart posEnd) -> min (posStart ^. lensVL _z) (posEnd ^. lensVL _z))

naiveDisintegrate :: S.Set Brick -> S.Set Brick
naiveDisintegrate stableBricks = flip S.filter stableBricks $ \brick ->
  simulateFalling (S.toList $ S.delete brick stableBricks) == S.delete brick stableBricks


naiveChainReaction :: S.Set Brick -> [Int]
naiveChainReaction stableBricks = S.toList stableBricks <&> \brick ->
  S.size $ simulateFalling (S.toList $ S.delete brick stableBricks) `S.difference` stableBricks

solution1 :: Input -> IO ()
solution1 input =
  print $ length $ naiveDisintegrate $ simulateFalling input

solution2 :: Input -> IO ()
solution2 input = print $ sum $ naiveChainReaction $ simulateFalling input

main :: IO ()
main = do
  input <- readInput
  solution1 input
  solution2 input
