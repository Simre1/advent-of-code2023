module Day19.Main where

import Control.Applicative ((<|>))
import Control.Applicative.Combinators qualified as P hiding (sepBy)
import Data.Bifunctor (Bifunctor (..))
import Data.List qualified as L
import Data.List.Split qualified as LS
import Data.Map qualified as M
import Data.Range
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Shared
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P

type Input = ([Workflow], [Part])

data Workflow = Workflow Text [Rule] deriving (Eq, Show, Ord)

data Destination = Accept | Reject | AnotherWorkflow Text deriving (Eq, Ord, Show)

data Rule
  = Greater PartProperty Int Destination
  | Lesser PartProperty Int Destination
  | Otherwise Destination
  deriving (Eq, Show, Ord)

data PartProperty = PPA | PPX | PPM | PPS deriving (Eq, Show, Ord)

data PartRange = PartRange
  { x :: Range Int,
    m :: Range Int,
    a :: Range Int,
    s :: Range Int
  }
  deriving (Eq, Show)

data Part = Part
  { x :: Int,
    m :: Int,
    a :: Int,
    s :: Int
  }
  deriving (Eq, Show, Ord)

setPartProperty :: PartProperty -> Int -> Part -> Part
setPartProperty pp amount part = case pp of
  PPX -> part {x = amount}
  PPM -> part {m = amount}
  PPA -> part {a = amount}
  PPS -> part {s = amount}

readPartProperty :: PartProperty -> Part -> Int
readPartProperty PPX part = part.x
readPartProperty PPM part = part.m
readPartProperty PPA part = part.a
readPartProperty PPS part = part.s

readInput :: IO Input
readInput = parseFileMega "inputs/day19/input" $ do
  workflows <- lexemeFull $ many1 workflow
  parts <- many1 part
  pure (workflows, parts)
  where
    workflow = do
      name <- wordP
      P.char '{'
      rules <- P.sepBy (ruleNormal <|> lastRule) (P.char ',')
      P.char '}'
      P.newline
      pure $ Workflow name rules

    ruleNormal = P.try $ do
      pp <- partProperty
      ruleType <- Lesser <$ P.char '<' <|> Greater <$ P.char '>'
      amount <- numberP
      P.char ':'
      ruleType pp amount <$> destination
    lastRule = Otherwise <$> destination
    partProperty = PPA <$ P.char 'a' <|> PPX <$ P.char 'x' <|> PPM <$ P.char 'm' <|> PPS <$ P.char 's'
    destination = Accept <$ P.char 'A' <|> Reject <$ P.char 'R' <|> AnotherWorkflow <$> wordP
    part = do
      P.char '{'
      setters <-
        P.sepBy
          ( do
              pp <- partProperty
              P.char '='
              amount <- numberP
              pure (pp, amount)
          )
          (P.char ',')
      P.char '}'
      P.newline
      pure $ L.foldl' (flip $ uncurry setPartProperty) (Part 0 0 0 0) setters

findAcceptedParts :: [Workflow] -> [Part] -> [Part]
findAcceptedParts workflowList = filter (isAccepted "in")
  where
    isAccepted workflowName part =
      let Workflow _ rules = workflows M.! workflowName
       in case getDestination part rules of
            Accept -> True
            Reject -> False
            AnotherWorkflow name -> isAccepted name part

    workflows = M.fromList $ (\wf@(Workflow name _) -> (name, wf)) <$> workflowList
    getDestination part [] = error "oh no"
    getDestination part (rule : rules) = case rule of
      Greater pp amount destination ->
        if readPartProperty pp part > amount
          then destination
          else getDestination part rules
      Lesser pp amount destination ->
        if readPartProperty pp part < amount
          then destination
          else getDestination part rules
      Otherwise destination -> destination

atRange :: PartProperty -> PartRange -> (Range Int -> (Range Int, Range Int)) -> (PartRange, PartRange)
atRange pp partRange f = case pp of
  PPX -> let fill v = partRange {x = v} :: PartRange in bimap fill fill $ f partRange.x
  PPM -> let fill v = partRange {m = v} :: PartRange in bimap fill fill $ f partRange.m
  PPA -> let fill v = partRange {a = v} :: PartRange in bimap fill fill $ f partRange.a
  PPS -> let fill v = partRange {s = v} :: PartRange in bimap fill fill $ f partRange.s

findAcceptedRanges :: [Workflow] -> [PartRange]
findAcceptedRanges workflowList = go (workflows M.! "in") $ PartRange initialRange initialRange initialRange initialRange
  where
    go :: Workflow -> PartRange -> [PartRange]
    go (Workflow _ rules) partRange = do
      (destination, partRange) <- applyRules partRange rules
      case destination of
        Accept -> [partRange]
        Reject -> []
        AnotherWorkflow name -> go (workflows M.! name) partRange

    applyRules :: PartRange -> [Rule] -> [(Destination, PartRange)]
    applyRules partRange (r : rs) = case r of
      Greater pp amount destination ->
        let (below, above) = atRange pp partRange $ \r ->
              (head $ intersection [r] [1 +=+ amount], head $ intersection [r] [amount *=+ 4000])
         in (destination, above) : applyRules below rs
      Lesser pp amount destination ->
        let (below, above) = atRange pp partRange $ \r ->
              (head $ intersection [r] [1 +=* amount], head $ intersection [r] [amount +=+ 4000])
         in (destination, below) : applyRules above rs
      Otherwise dest -> [(dest, partRange)]
    initialRange = 1 +=+ 4000
    workflows = M.fromList $ (\wf@(Workflow name _) -> (name, wf)) <$> workflowList

rangeLength :: Range Int -> Int
rangeLength (SpanRange (Bound l Inclusive) (Bound r Inclusive)) = r - (l - 1)
rangeLength (SpanRange (Bound l Exclusive) (Bound r Inclusive)) = r - l
rangeLength (SpanRange (Bound l Inclusive) (Bound r Exclusive)) = (r - 1) - (l - 1)
rangeLength (SpanRange (Bound l Exclusive) (Bound r Exclusive)) = (r - 1) - l

partPossibilities :: PartRange -> Int
partPossibilities (PartRange x m a s) = product $ rangeLength <$> [x, m, a, s]

partRating :: Part -> Int
partRating (Part x m a s) = x + m + a + s

solution1 :: Input -> IO ()
solution1 input =
  print $ sum $ partRating <$> uncurry findAcceptedParts input

solution2 :: Input -> IO ()
solution2 (workflows, _) = print $ sum $ partPossibilities <$> findAcceptedRanges workflows

main :: IO ()
main = do
  input <- readInput
  solution1 input
  solution2 input
