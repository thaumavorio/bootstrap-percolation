module DagAlgo (dagAlgo) where

import Data.List (minimumBy)

import Data.Graph.Types
import Data.Graph.DGraph

dagAlgo :: DGraph Int () -> Int -> [Int]
dagAlgo graph threshold = dagAlgoItr graph graph
  where
    dagAlgoItr unproc acyclic
      | null $ vertices unproc = filter (\v -> vertexIndegree acyclic v < threshold) $ vertices acyclic
      | otherwise = dagAlgoItr (removeVertex minInVert unproc) . flip removeArcs acyclic $ inboundingArcs unproc minInVert
      where
        minInVert = minimumBy (\v1 v2 -> compare (vertexIndegree unproc v1) (vertexIndegree unproc v2)) $ vertices unproc
