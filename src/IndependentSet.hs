module IndependentSet (independentSetGreedy, caroWeiBound, selkowBound, henryISBound, newGreedy, newGreedy2) where

import BootstrapPercolation

import Data.Graph.Types
import Data.Graph.UGraph
import Data.Ratio ((%))
import Data.List (delete, maximumBy, minimumBy, sort)

independentSetGreedyItr :: UGraph Int () -> [Int] -> [Int]
independentSetGreedyItr subgraph acc
  | null $ vertices subgraph = acc
  | otherwise                = independentSetGreedyItr (removeVertices toRemove subgraph) $ v : acc
  where
    v = minimumBy (\v1 v2 -> compare (vertexDegree subgraph v1) (vertexDegree subgraph v2)) $ vertices subgraph
    toRemove = v : adjacentVertices subgraph v

independentSetGreedy :: UGraph Int () -> [Int]
independentSetGreedy graph = independentSetGreedyItr graph []

caroWeiBound :: UGraph Int () -> Rational
caroWeiBound graph = sum . map (\v -> 1 % fromIntegral (1 + vertexDegree graph v)) $ vertices graph

selkowBound :: UGraph Int () -> Rational
selkowBound graph = sum . map (\v -> (1 + max 0 (fromIntegral (degree v) % fromIntegral (1 + degree v) - localSum v)) * (1 % fromIntegral (1 + degree v))) $ vertices graph
  where
    degree v = vertexDegree graph v
    localSum v = sum . map (\u -> 1 % fromIntegral (1 + degree u)) $ adjacentVertices graph v

henryISBoundV :: UGraph Int () -> Int -> Rational
henryISBoundV graph v = if null $ a v then 1 % fromIntegral (1 + length (b v)) else 0
  where
    b v = filter (\u -> vertexDegree graph u <= vertexDegree graph v) $ adjacentVertices graph v
    a v = filter (\u -> vertexDegree graph u < vertexDegree graph v) $ adjacentVertices graph v

henryISBound :: UGraph Int () -> Rational
henryISBound graph = sum . map (henryISBoundV graph) $ vertices graph

newGreedyItr :: UGraph Int () -> [Int] -> [Int]
newGreedyItr subgraph acc
  | null $ vertices subgraph = acc
  | otherwise                = newGreedyItr (removeVertices toRemove subgraph) $ v : acc
  where
    vs = filter (\v -> vertexDegree subgraph v == minDegree subgraph) $ vertices subgraph
    v = maximumBy (\v1 v2 -> compare (henryISBoundV subgraph v1) (henryISBoundV subgraph v2)) vs
    toRemove = v : adjacentVertices subgraph v

newGreedy :: UGraph Int () -> [Int]
newGreedy graph = newGreedyItr graph []

newGreedy2Itr :: UGraph Int () -> [Int] -> [Int]
newGreedy2Itr subgraph acc
  | null $ vertices subgraph = acc
  | otherwise                = newGreedy2Itr (removeVertices toRemove subgraph) $ v : acc
  where
    v = maximumBy (\v1 v2 -> compare (henryISBoundV subgraph v1) (henryISBoundV subgraph v2)) $ vertices subgraph
    toRemove = v : adjacentVertices subgraph v

newGreedy2 :: UGraph Int () -> [Int]
newGreedy2 graph = newGreedyItr graph []
