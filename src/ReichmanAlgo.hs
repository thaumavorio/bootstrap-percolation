module ReichmanAlgo where

import BootstrapPercolation

import Data.Graph.Types
import Data.Graph.UGraph

import Data.List (delete, minimumBy)

{-|
 - Perform the greedy algorithm given in "New bounds for contagious sets".
 - Returns a contagious set which is not guaranteed to be minimal.
 -}
reichmanAlgo :: UGraph Int () -> Int -> [Int]
reichmanAlgo graph threshold = reichmanAlgoItr graph threshold $ vertices graph
  where
    reichmanAlgoItr subgraph threshold acc
      | null removableVertices = acc
      | otherwise              = reichmanAlgoItr (removeVertex v subgraph) threshold $ delete v acc
      where
        removableVertices = filter (\v -> vertexDegree subgraph v >= threshold) $ vertices subgraph
        v = minimumBy (\v1 v2 -> compare (vertexDegree subgraph v1) (vertexDegree subgraph v2)) removableVertices
