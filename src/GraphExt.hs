module GraphExt (subgraph, gridGraph, triangleGraph) where

import Data.Graph.Types
import Data.Graph.UGraph

import Data.List ((\\))
import Data.List.Split (chunksOf)

import Data.Hashable

import Data.Maybe (fromMaybe)
import Control.Monad (replicateM)

{-|
 - Return the subgraph of the given graph induced by the given vertices.
 -}
subgraph :: (Ord v, Hashable v, Eq v) => UGraph v e -> [v] -> UGraph v e
subgraph graph vs = removeVertices (vertices graph \\ vs) graph

{-|
 - Return the m by n grid graph. That is, the graph product of a path of length
 - m by a path of length n.
 -}
gridGraph :: Int -> Int -> UGraph Int ()
gridGraph m n = insertEdgePairs vertEdges . insertEdgePairs horizEdges $ insertVertices vertices empty
  where
    vertices = [0..m*n-1]
    horizEdges = [ (v,v+1) | v <- filter (\v -> (v + 1) `rem` m /= 0) vertices ]
    vertEdges = [ (v,v+m) | v <- take ((n-1)*m) vertices ]

tri :: Int -> Int
tri n = (n^2 + n) `div` 2

triangleGraph :: Int -> UGraph Int ()
triangleGraph 1 = insertVertex 0 empty
triangleGraph 2 = insertEdgePairs [(0,1), (0,2), (1,2)] empty
triangleGraph n = insertEdgePairs verEdges $ insertEdgePairs horEdges prevGraph
  where
    prevGraph = triangleGraph $ n - 1
    curTri = tri n
    prevTri = tri $ n - 1
    pPrevTri = tri $ n - 2
    horEdges = zip [prevTri..curTri-2] [prevTri+1..curTri-1]
    verEdges = concatMap (\v -> [(v,v+n-1),(v,v+n)]) [pPrevTri..prevTri-1]
