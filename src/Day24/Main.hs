module Day24.Main where

import Control.Applicative.Combinators qualified as P
import Control.Monad (guard, unless)
import Data.Either (fromRight)
import Data.Foldable (forM_)
import Data.List qualified as L
import Data.List.Split qualified as LS
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Data.SBV qualified as SBV
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Debug.Trace (traceShow, traceShowId)
import Linear.V2
import Linear.V3
import Math.MFSolve qualified as Solve
import Shared
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P
import Data.Traversable (for)

type Input = [Hailstone]

data Hailstone = Hailstone
  { pos :: V3 Double,
    vel :: V3 Double
  }
  deriving (Eq, Show, Ord)

readInput :: IO Input
readInput = parseFileMega "inputs/day24/example" $ parseLines $ do
  pos <- v3
  lexeme $ P.char '@'
  Hailstone pos <$> v3
  where
    v3 = do
      x <- lexeme signedNumberP
      lexeme $ P.char ','
      y <- lexeme signedNumberP
      lexeme $ P.char ','
      z <- lexeme signedNumberP
      pure $ fromIntegral <$> V3 x y z

intersectHailstones :: Bool -> Hailstone -> Hailstone -> Maybe (V2 Double)
intersectHailstones ignoreZ (Hailstone pos1 vel1) (Hailstone pos2 vel2) = either (const Nothing) pure $ flip Solve.evalSolver Solve.noDeps $ do
  let [t1, t2] = map (Solve.makeVariable @Double . Solve.SimpleVar) ["t1", "t2"]
  x1 + dx1 * t1 Solve.=== x2 + dx2 * t2
  y1 + dy1 * t1 Solve.=== y2 + dy2 * t2
  unless ignoreZ $
      z1 + dz1 * t1 Solve.=== z2 + dz2 * t2

  t1Val <- Solve.getValue $ Solve.SimpleVar "t1"
  t2Val <- Solve.getValue $ Solve.SimpleVar "t2"
  pure $ V2 t1Val t2Val
  where
    (V3 x1 y1 z1) = Solve.makeConstant <$> pos1
    (V3 x2 y2 z2) = Solve.makeConstant <$> pos2
    (V3 dx1 dy1 dz1) = Solve.makeConstant <$> vel1
    (V3 dx2 dy2 dz2) = Solve.makeConstant <$> vel2

follow :: Hailstone -> Double -> Hailstone
follow (Hailstone pos vel) time = Hailstone (((time *) <$> vel) + pos) vel

inTestArea :: Int -> Int -> [Hailstone] -> [Hailstone]
inTestArea s e = filter (\(Hailstone (V3 x y z) _) -> x >= fromIntegral s && x <= fromIntegral e && y >= fromIntegral s && y <= fromIntegral e)

solution1 :: Input -> IO ()
solution1 input = do
  let intersections =
        [ case intersectHailstones True a b of
            Nothing -> Nothing
            Just (V2 t1 t2) -> if t1 >= 0 && t2 >= 0 then Just $ follow a t1 else Nothing
          | a <- input,
            b <- input,
            a < b
        ]
  print $ length $ inTest $ catMaybes intersections
  where
    -- inTest = inTestArea 7 27
    inTest = inTestArea 200000000000000 400000000000000

solution2 :: Input -> IO ()
solution2 input = 
  print $ x + y + z
  where
    x = 140604613634294
    y = 224390889669946
    z = 206098283112689

-- 194592040768564, 332365743938486, 196880917504399 @ 160, -81, 182
-- 119269259427296, 151358331038299, 32133087271013 @ 320, 350, 804
-- 137316267565914, 280950442046082, 163349784223749 @ 252, -89, 298

-- Extract first three hailstones

-- 194592040768564 + 160 * t1 = x + dx * t1
-- 332365743938486 - 81 * t1 = y + dy * t1
-- 196880917504399 + 182 * t1 = z + dz * t1

-- 119269259427296+ 320 * t2 = x + dx * t2
-- 151358331038299+ 350* t2 = y + dy * t2
-- 32133087271013 + 804* t2 = z + dz * t2

-- 137316267565914+ 252 * t2 = x + dx * t2
-- 280950442046082 - 89 * t2 = y + dy * t2
-- 163349784223749 + 298* t2 = z + dz * t2

-- 194592040768564 + 160 * u = x + d * u, 332365743938486 - 81 * u = y + f * u, 196880917504399 + 182 * u = z + g* u, 119269259427296+ 320 * i = x + d * i, 151358331038299+ 350* i = y + f * i, 32133087271013 + 804* i = z + g * i, 137316267565914+ 252 * o = x + d * o, 280950442046082 - 89 * o = y + f * o, 163349784223749 + 298* o = z + g * o
-- Solve on quickmath.com


main :: IO ()
main = do
  input <- readInput
  solution1 input
  solution2 input

