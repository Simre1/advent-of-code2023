module Day25.Main where

import Control.Applicative.Combinators qualified as P
import Data.List qualified as L
import Data.List.Split qualified as LS
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Shared
import System.Random (mkStdGen, uniformR)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P
import Data.GraphViz (graphElemsToDot, GraphvizParams (..), defaultParams, runGraphvizCanvas', GraphvizCanvas (..), nonClusteredParams)

type Input = [Component]

data Component = Component
  { name :: Text,
    connections :: [Text]
  }
  deriving (Eq, Show, Ord)

newtype ComponentGraph = ComponentGraph {graph :: M.Map Text (S.Set Text)} deriving (Eq, Show, Ord)

readInput :: IO Input
readInput = parseFileMega "inputs/day25/input" $ parseLines $ do
  name <- wordP
  lexeme $ P.char ':'
  outputs <- many1 $ lexeme wordP
  pure $ Component name outputs

buildGraph :: [Component] -> ComponentGraph
buildGraph components = ComponentGraph $ go M.empty components
  where
    go graph [] = graph
    go graph (Component name connections : cs) =
      let newGraph = L.foldl' (\g c -> insert name c $ insert c name g) graph connections
       in go newGraph cs
    insert a b = M.alter (maybe (Just $ S.singleton b) (Just . S.insert b)) a

moveRandomly :: Int -> ComponentGraph -> M.Map (Text, Text) Int
moveRandomly steps (ComponentGraph graph) = go (mkStdGen 0) (head $ M.keys graph) steps
  where
    go gen current 0 = M.empty
    go gen current steps =
      let next = graph M.! current
          (i, nextGen) = uniformR (0, S.size next - 1) gen
       in M.alter (maybe (Just 1) (Just . succ)) (current, S.toList next !! i) $ go nextGen (S.toList next !! i) (steps - 1)

cutEdge :: (Text, Text) -> ComponentGraph -> ComponentGraph
cutEdge (a,b) (ComponentGraph graph) = ComponentGraph $ M.update (Just . S.delete b) a $
  M.update (Just . S.delete a) b graph

solution1 :: Input -> IO ()
solution1 input = do
  let componentGraph@(ComponentGraph graph) = buildGraph input
  let edges = mconcat $ (\(name, connections) -> (\c -> (name, c, name <> c)) <$> S.toList connections ) <$> M.toList graph
      dotGraph = graphElemsToDot (nonClusteredParams {isDirected=False}) [] edges
  runGraphvizCanvas' dotGraph Gtk

  let disconnect = [("fzb", "fxr"), ("thl", "nmv"), ("mbq", "vgk")] -- view graphviz and determine edges
      disconnectedGraph = L.foldl (flip cutEdge) componentGraph disconnect

  print $ product (S.size <$> connectedComponents disconnectedGraph)

dfs :: ComponentGraph -> Text -> S.Set Text
dfs g@(ComponentGraph graph) = go S.empty 
  where
    go visited current =
        let neighbors = M.findWithDefault S.empty current graph
            newVisited = S.insert current visited
            unvisited = S.difference neighbors newVisited
        in L.foldl' go newVisited $ S.toList unvisited

connectedComponents :: ComponentGraph -> [S.Set Text]
connectedComponents g@(ComponentGraph graph) =
  let allNodes = M.keysSet graph
      go [] acc = acc
      go (node : rest) acc =
        let component = dfs g node
        in go (S.toList (S.difference (S.fromList rest) component)) (component : acc)
  in go (S.toList allNodes) []

solution2 :: Input -> IO ()
solution2 input = putStrLn "PUSH BIG RED BUTTON"

main :: IO ()
main = do
  input <- readInput
  solution1 input
  solution2 input
