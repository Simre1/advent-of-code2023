module Day5.Main where

import Control.Applicative.Combinators qualified as P
import Control.Monad (forM_)
import Data.Functor ((<&>))
import Data.List qualified as L
import Data.List.Split qualified as LS
import Data.Map qualified as M
import Data.SBV
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Debug.Trace
import Shared
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P

newtype Seeds = Seeds [Int] deriving (Eq, Show, Ord)

data Mapping = Mapping
  { from :: Text,
    to :: Text,
    ranges :: [(Int, Int, Int)]
  }
  deriving (Show, Eq, Ord)

type Input = (Seeds, M.Map Text Mapping)

readInput :: IO Input
readInput = parseFileMega "inputs/day5/input" $ do
  seeds' <- seeds
  mappings <- many1 mapping
  pure (seeds', M.fromList $ fmap (\m -> (m.from, m)) mappings)
  where
    seeds =
      Seeds <$> do
        lexemeFull $ P.string "seeds:"
        many1 $ lexemeFull numberP
    mapping = do
      from <- P.takeWhile1P Nothing (/= '-')
      P.string "-to-"
      to <- P.takeWhile1P Nothing (/= ' ')
      lexemeFull $ P.string " map:"
      ranges <- many1 $ P.try $ do
        destination <- lexemeFull numberP
        source <- lexemeFull numberP
        range <- lexemeFull numberP
        pure (destination, source, range)
      pure $ Mapping from to ranges

oneOf :: SInt64 -> [SInt64] -> SBool
oneOf x = L.foldl' (\b a -> b .|| a .== x) sFalse

solveZ3 :: (SInt64 -> SBool) -> M.Map Text Mapping -> IO ()
solveZ3 seedConstraint mappings = do
  result <- optimize Independent $ do
    let varnames = S.fromList $ fmap (.to) (M.elems mappings) ++ fmap (.from) (M.elems mappings)
    vars <-
      fmap M.fromList $
        traverse (\name -> (name,) <$> sInt64 (T.unpack name)) $
          S.toList varnames

    let seed = vars M.! "seed"

    constrain $ seedConstraint seed
    forM_ mappings $ \mapping -> do
      let from = vars M.! mapping.from
          to = vars M.! mapping.to
      constrain $
        sOr
          [ sOr $
              mapping.ranges <&> \(destination, source, rangeSize) ->
                let fromConstraint = from .>= fromIntegral source .&& from .< fromIntegral (source + rangeSize)
                 in fromConstraint .&& (to - fromIntegral destination .== from - fromIntegral source),
            sNot
              ( sOr $
                  mapping.ranges <&> \(destination, source, rangeSize) ->
                    let fromConstraint = from .>= fromIntegral destination .&& from .< fromIntegral (destination + rangeSize)
                     in fromConstraint
              )
              .&& (from .== to)
          ]

    minimize "Minimize Location" $ vars M.! "location"

  print result

pairUp :: [a] -> [(a, a)]
pairUp [] = []
pairUp [_] = []
pairUp (x : y : xs) = (x, y) : pairUp xs

solution1 (Seeds seeds, mappings) =
  solveZ3
    (\seed -> seed `oneOf` (fromIntegral <$> seeds))
    mappings

solution2 :: Input -> IO ()
solution2 (Seeds seeds, mappings) =
  let seedRanges = pairUp seeds
   in solveZ3
        (\seed -> sOr $ (\(start, range) -> seed .>= fromIntegral start .&& seed .< fromIntegral (start + range)) <$> seedRanges)
        mappings

main :: IO ()
main = do
  input <- readInput
  solution1 input
  solution2 input
