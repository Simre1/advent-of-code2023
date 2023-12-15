module Day15.Main where

import Control.Monad (void)
import Data.List qualified as L
import Data.List.Split qualified as LS
import Data.Map qualified as M
import Data.Monoid (Sum (..))
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Shared
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P

type Input = [Instruction]

data Instruction = Assign Text Int | Minus Text deriving (Eq, Show, Ord)

newtype Box = Box [(Text, Int)] deriving (Eq, Show, Ord)

newtype Boxes = Boxes (M.Map Int Box) deriving (Eq, Show, Ord)

modifyBox :: Int -> (Box -> Box) -> Boxes -> Boxes
modifyBox key f (Boxes boxes) = Boxes $ M.alter (Just . maybe (f $ Box []) f) key boxes

addToBox :: Text -> Int -> Box -> Box
addToBox label focalLength (Box lenses) = Box $
  case L.lookup label lenses of
    Just x -> replaceList (label, x) (label, focalLength) lenses
    Nothing -> (label, focalLength) : lenses

deleteFromBox :: Text -> Box -> Box
deleteFromBox label (Box lenses) = Box $ filter ((/= label) . fst) lenses

emptyBoxes :: Boxes
emptyBoxes = Boxes M.empty

computeFocusingPower :: Boxes -> Int
computeFocusingPower (Boxes boxes) = getSum $ flip foldMap (M.toList boxes) $ \(boxIndex, Box lenses) ->
  let lensesN = length lenses
   in flip foldMap (zip [lensesN, lensesN - 1 ..] lenses) $ \(slotNumber, (label, focalLength)) ->
        Sum $ (boxIndex + 1) * slotNumber * focalLength

handleInstruction :: Instruction -> Boxes -> Boxes
handleInstruction (Assign label focalLength) boxes =
  let boxIndex = hashText label
   in modifyBox boxIndex (addToBox label focalLength) boxes
handleInstruction (Minus label) boxes =
  let boxIndex = hashText label
   in modifyBox boxIndex (deleteFromBox label) boxes

readInput :: IO Input
readInput = parseFileMega "inputs/day15/input" $ do
  let instructionP = do
        w <- wordP
        P.choice
          [ P.try $ do
              P.char '='
              Assign w <$> numberP,
            do
              P.try $ P.char '-'
              pure $ Minus w
          ]
  instructionP `P.sepBy` P.char ','

hashText :: Text -> Int
hashText = hash . T.unpack

hashInstruction :: Instruction -> String
hashInstruction (Assign w i) = T.unpack w ++ "=" ++ show i
hashInstruction (Minus w) = T.unpack w ++ "-"

hash :: String -> Int
hash = L.foldl' (\current char -> (fromEnum char + current) * 17 `mod` 256) 0

solution1 :: Input -> IO ()
solution1 input =
  print $ sum $ hash . hashInstruction <$> input

solution2 :: Input -> IO ()
solution2 input = do
  print $ computeFocusingPower $ L.foldl' (flip handleInstruction) emptyBoxes input

main :: IO ()
main = do
  input <- readInput
  solution1 input
  solution2 input
