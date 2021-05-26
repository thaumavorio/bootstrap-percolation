module BootstrapPercolation where

import Data.Graph.Types
import Data.Graph.UGraph

import Data.List (intersect)

import Data.Ratio ((%))

import Data.Maybe (fromJust)

import qualified Data.HashMap.Strict as HM

{-|
 - The upper bound for the number of vertices in a minimal contagious set in
 - the given graph with the given threshold, as stated in "New bounds for contagious sets".
 -
 - TODO: cite properly
 -}
reichmanBound :: UGraph Int () -> Int -> Rational
reichmanBound graph threshold = sum . map vertVal $ vertices graph
  where
    vertVal v = min 1 (fromIntegral threshold % fromIntegral (vertexDegree graph v + 1))

{-|
 - The upper bound for the number of vertices in a minimal contagious set in
 - the given graph with the given threshold, as discovered by us. This is our
 - deterministic bound.
 -}
henryBound :: UGraph Int () -> Int -> Rational
henryBound graph threshold = sum . map vertVal $ vertices graph
  where
    vertVal v
      | degV < threshold = 1
      | degV >= threshold && null nV = 0
      | otherwise = fromIntegral threshold % fromIntegral (degV + 1)
      where
        degV = vertexDegree graph v
        nV = filter (\u -> vertexDegree graph u >= threshold) $ adjacentVertices graph v

{-|
 - The upper bound for the number of vertices in a minimal contagious set in
 - the given graph with the given threshold, as discovered by us. This is our
 - probabilistic bound.
 -}
henryBound2 :: UGraph Int () -> Int -> Rational
henryBound2 graph threshold = sum . map vertVal $ vertices graph
  where
    vertVal v
      | hb v < threshold = 1
      | ha v < threshold = fromIntegral (threshold - ha v) % fromIntegral (1 + (hb v - ha v))
      | otherwise = 0
    ha v = length $ filter (\u -> vertexDegree graph u < vertexDegree graph v) $ adjacentVertices graph v
    hb v = length $ filter (\u -> vertexDegree graph u <= vertexDegree graph v) $ adjacentVertices graph v

{-|
 - The upper bound for the number of vertices in a minimal contagious set in
 - the given graph with the given threshold, as discovered by us. This is our
 - second probabilistic bound.
 -}
henryBound3 :: UGraph Int () -> Int -> Rational
henryBound3 graph threshold = sum . map vertVal $ vertices graph
  where
    vertVal v
      | hd v < threshold = 1
      | hc v < threshold = fromIntegral (threshold - hc v) % fromIntegral (1 + (hd v - hc v))
      | otherwise = 0
    hc v = length $ filter (\u -> vertexDegree graph u > vertexDegree graph v) $ adjacentVertices graph v
    hd v = length $ filter (\u -> vertexDegree graph u >= vertexDegree graph v) $ adjacentVertices graph v

{-|
 - Perform bootstrap percolation iterations on the given graph with the given
 - constant threshold starting with the given seed set. Returns the first set
 - which, when ran through another iteration, returns the same set; that is,
 - the first set which activates no further vertices.
 -}
perc :: UGraph Int () -> Int -> [Int] -> [Int]
perc graph threshold seeds
  | length seeds == length perc_itr = perc_itr
  | otherwise = perc graph threshold perc_itr
  where
    perc_itr = filter (\v -> v `elem` seeds || threshold <= (length . intersect seeds $ adjacentVertices graph v)) $ vertices graph

{-|
 - Perform bootstrap percolation iterations on the given graph with the given
 - threshold function starting with the given seed set. Returns the first set
 - which, when ran through another iteration, returns the same set; that is,
 - the first set which activates no further vertices.
 -}
percFunc :: UGraph Int () -> HM.HashMap Int Int -> [Int] -> [Int]
percFunc graph threshold seeds
  | length seeds == length perc_itr = perc_itr
  | otherwise = percFunc graph threshold perc_itr
  where
    perc_itr = filter (\v -> v `elem` seeds || fromJust (HM.lookup v threshold) <= (length . intersect seeds $ adjacentVertices graph v)) $ vertices graph
