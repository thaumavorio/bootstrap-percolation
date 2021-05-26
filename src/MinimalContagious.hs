module MinimalContagious (minimumContagiousSet, minimumContagious) where

import BootstrapPercolation
import GraphExt

import Data.Graph.Types
import Data.Graph.UGraph

import Data.Maybe (fromMaybe, isNothing, fromJust)
import Data.List (delete)

import qualified Data.HashMap.Strict as HM

{-|
 - Find a minimal contagious set by our recursive algorithm, given a constant threshold.
 -}
minimumContagious :: UGraph Int () -> Int -> [Int]
minimumContagious graph k = minimumContagiousSet graph thresholds
  where
    thresholds = foldr (`HM.insert` k) HM.empty $ vertices graph

{-|
 - Find a minimal contagious set by our recursive algorithm, given a threshold
 - function.
 -}
minimumContagiousSet :: UGraph Int () -> HM.HashMap Int Int -> [Int]
minimumContagiousSet graph thresholds = minimumContagiousSetItr graph updatedThresholds requiredVertices
  where
    requiredVertices = filter (\v -> vertexDegree graph v < fromJust (HM.lookup v thresholds)) $ vertices graph
    updatedThresholds = foldr (HM.adjust (const 0)) thresholds requiredVertices

minimumContagiousSetItr :: UGraph Int () -> HM.HashMap Int Int -> [Int] -> [Int]
minimumContagiousSetItr graph thresholds requiredVertices
  | not $ null possible = minimumContagiousSetItr nextGraph nextThresholds requiredVertices
  | otherwise = requiredVertices ++ fromMaybe (vertices graph) (restrictedContagiousSet graph thresholds $ vertices graph)
  where
    possible = filter (\v -> fromJust (HM.lookup v thresholds) <= 0) $ vertices graph
    v = head possible
    nextGraph = removeVertex v graph
    nextThresholds = HM.delete v . foldr (HM.adjust (subtract 1)) thresholds $ adjacentVertices graph v -- "Fine, we'll keep that" - Henry, 11 October 2020 21:25

restrictedContagiousSet :: UGraph Int () -> HM.HashMap Int Int -> [Int] -> Maybe [Int]
restrictedContagiousSet graph thresholds considerable
  | null possible && length percConsiderable == order graph = Just considerable
  | null possible = Nothing
  | isNothing setWithV = Nothing
  | isNothing nextCSet = setWithV
  | length (fromJust setWithV) < length (fromJust nextCSet) = setWithV
  | otherwise = nextCSet
  where
    possible = filter (\v -> vertexDegree graph v >= fromJust (HM.lookup v thresholds)) considerable
    percConsiderable = percFunc graph thresholds considerable
    v = head possible
    setWithV = minimumContagiousSetWithV graph thresholds considerable v
    nextCSet = restrictedContagiousSet graph thresholds $ delete v considerable

minimumContagiousSetWithVItr :: UGraph Int () -> HM.HashMap Int Int -> [Int] -> Int -> Maybe [Int]
minimumContagiousSetWithVItr graph thresholds considerable v
  | not $ null possible = minimumContagiousSetWithVItr newGraph newThresholds newConsiderable v
  | isNothing nextCSet = Nothing
  | otherwise = Just (v : fromJust nextCSet)
  where
    possible = filter (\u -> fromJust (HM.lookup u thresholds) <= 0) $ vertices graph
    u = head possible
    thresholds1 = foldr (HM.adjust (\kw -> kw - 1)) thresholds $ adjacentVertices graph u
    newGraph = removeVertex u graph
    newThresholds = HM.delete u thresholds1
    newConsiderable = delete u considerable
    nextCSet = restrictedContagiousSet graph thresholds considerable

minimumContagiousSetWithV :: UGraph Int () -> HM.HashMap Int Int -> [Int] -> Int -> Maybe [Int]
minimumContagiousSetWithV graph thresholds considered v = minimumContagiousSetWithVItr graph updatedThresholds considered v
  where
    updatedThresholds = HM.adjust (const 0) v thresholds
