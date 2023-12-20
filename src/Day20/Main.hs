module Day20.Main where

import Control.Applicative.Combinators qualified as P hiding (sepBy)
import Data.Bifunctor (Bifunctor (..))
import Data.Functor ((<&>))
import Data.List qualified as L
import Data.List.Split qualified as LS
import Data.Map qualified as M
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Debug.Trace
import Shared
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P

type Input = Network

data Network = Network
  { connections :: !(M.Map Text Module),
    broadcaster :: ![Text],
    outputs :: ![Bool]
  }
  deriving (Eq, Show, Ord)

data Module = Module
  { name :: !Text,
    mType :: !ModuleType,
    outputs :: ![Text]
  }
  deriving (Eq, Show, Ord)

data ModuleType = Flipflop {onOff :: !Bool} | Conjunction {rememberedInputs :: !(M.Map Text Bool)} deriving (Eq, Show, Ord)

readInput :: IO Input
readInput = parseFileMega "inputs/day20/input" $ fmap (connectConjunctions . fillNetwork) $ parseLines $ do
  P.choice
    [ do
        P.char '%'
        name <- lexeme wordP
        lexeme $ P.string "->"
        outputs <- P.sepBy (lexeme wordP) (lexeme $ P.char ',')
        pure $ \n -> n {connections = M.insert name (Module name (Flipflop False) outputs) n.connections},
      do
        P.char '&'
        name <- lexeme wordP
        lexeme $ P.string "->"
        outputs <- P.sepBy (lexeme wordP) (lexeme $ P.char ',')
        pure $ \n -> n {connections = M.insert name (Module name (Conjunction M.empty) outputs) n.connections},
      do
        lexeme $ P.string "broadcaster"
        lexeme $ P.string "->"
        outputs <- P.sepBy (lexeme wordP) (lexeme $ P.char ',')
        pure $ \n -> n {broadcaster = outputs}
    ]
  where
    fillNetwork = L.foldl' (\b f -> f b) (Network M.empty [] [])
    connectConjunctions :: Network -> Network
    connectConjunctions network = L.foldl' connect network $ M.toList network.connections
    connect network (name, mod) = case mod.mType of
      Flipflop _ -> network
      Conjunction _ ->
        network
          { connections =
              M.insert
                name
                ( mod
                    { mType = Conjunction $ M.fromList (map (,False) (findAllInputs network name))
                    }
                )
                network.connections
          }
    findAllInputs :: Network -> Text -> [Text]
    findAllInputs network name =
      fst
        <$> filter
          (\(_, outputs) -> name `elem` outputs)
          ( ("broadcaster", network.broadcaster)
              : ( second (\mod -> mod.outputs)
                    <$> M.toList
                      network.connections
                )
          )

pressButton :: forall a. (a -> Network -> (Text, Bool, Text) -> a) -> a -> Network -> (Network, a)
pressButton accumulate acc network = sendUntilDone acc network $ Seq.fromList $ ("broadcaster",False,) <$> network.broadcaster
  where
    sendUntilDone :: a -> Network -> Seq.Seq (Text, Bool, Text) -> (Network, a)
    sendUntilDone a network Seq.Empty = (network, a)
    sendUntilDone a network ((f, p, t) Seq.:<| ts) =
      let (newNet, newTargets) = sendOne f p t network
       in sendUntilDone (accumulate a network (f, p, t)) newNet (ts <> newTargets)

    sendOne :: Text -> Bool -> Text -> Network -> (Network, Seq.Seq (Text, Bool, Text))
    sendOne from pulse target network =
      case M.lookup target network.connections of
        Just mod -> case mod.mType of
          Flipflop onOff ->
            if pulse
              then (network, Seq.empty)
              else
                let newFlipflop = Flipflop $ not onOff
                    sendOutput = (target,not onOff,) <$> Seq.fromList mod.outputs
                 in (network {connections = M.insert target (mod {mType = newFlipflop}) network.connections}, sendOutput)
          Conjunction rememberedInputs ->
            let newRemembered = M.insert from pulse rememberedInputs
                outPulse = (target,not (and newRemembered),) <$> Seq.fromList mod.outputs
             in (network {connections = M.insert target (mod {mType = Conjunction newRemembered}) network.connections}, outPulse)
        Nothing -> (network {outputs = pulse : network.outputs}, Seq.empty)

solution1 :: Input -> IO ()
solution1 input =
  let (l, h) =
        snd $
          applyNTimes
            1000
            (\(network, counts) -> pressButton (\(l, h) _ (_, pulse, _) -> if pulse then (l, succ h) else (succ l, h)) (first succ counts) network)
            (input, (0, 0))
   in print $ l * h

solution2 :: Input -> IO ()
solution2 input = do
  let finalModule = "zg" -- zg = module before my output, you need to adapt for your input file
      (Module _ (Conjunction finalInputs) _) = input.connections M.! finalModule

      activeTimes =
        M.keys finalInputs <&> \finalInput ->
              length $ takeWhile (\(_, done) -> not done) $
                L.iterate'
                  ( \(net, done) ->
                      let (!nextNet, !nextDone) = pressButton (\a _ (f, p, t) -> a || (t == finalModule && f == finalInput && p)) False net in (nextNet, nextDone)
                  )
                  (input, False)

  print $ L.foldl1 lcm activeTimes

main :: IO ()
main = do
  input <- readInput
  solution1 input
  solution2 input
