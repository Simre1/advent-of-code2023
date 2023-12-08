module Day8.Main where

import Control.Applicative ((<|>))
import Control.Applicative.Combinators qualified as P
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
import Data.Functor ((<&>))

type Input = ([Instruction], M.Map Field (Field, Field))

newtype Field = Field Text deriving (Eq, Show, Ord)

data Instruction = ILeft | IRight deriving (Eq, Show, Ord)

chooseDirection :: Instruction -> (a,a) -> a
chooseDirection ILeft (a,_) = a
chooseDirection IRight (_,a) = a

infiniteInstructions :: [Instruction] -> [Instruction]
infiniteInstructions = cycle

readInput :: IO Input
readInput = parseFileMega "inputs/day8/input" $ do
  instructions <- lexemeFull $ many1 $ ILeft <$ P.char 'L' <|> IRight <$ P.char 'R'
  fields <- many1 $ lexemeFull $ do
    place <- wordP
    lexeme $ P.char '='
    lexeme $ P.char '('
    left <- wordP
    lexeme $ P.char ','
    right <- wordP
    lexeme $ P.char ')'
    pure (Field place, (Field left, Field right))

  pure (instructions, M.fromList fields)

followInstructions :: M.Map Field (Field, Field) -> Field -> [Instruction] -> [Field]
followInstructions map current (i:is) =
  let currentDirections = map M.! current
      nextField = chooseDirection i currentDirections
  in current : followInstructions map nextField is

solution1 :: Input -> IO ()
solution1 input = do
  let (instructions, map) = input
      end = L.takeWhile (/= Field "ZZZ") $ followInstructions map (Field "AAA") $ infiniteInstructions instructions
  print $ length end

solution2 :: Input -> IO ()
solution2 input = do
  let (instructions, map) = input
      startFields = filter (\(Field field) -> T.isSuffixOf "A" field) $ M.keys map
      separatePaths = startFields <&> \startField -> 
        takeWhile (\(Field field) -> not $ T.isSuffixOf "Z" field) $ followInstructions map startField (infiniteInstructions instructions)
      -- pathToEnds = takeWhile (any $ \(Field field) -> not $ T.isSuffixOf "Z" field) infinitePaths
  print $ foldl1 leastCommonMultiple $ fmap length separatePaths

main :: IO ()
main = do
  input <- readInput
  solution1 input
  solution2 input
