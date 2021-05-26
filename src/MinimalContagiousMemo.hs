{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module MinimalContagiousMemo (minimumContagiousSet, minimumContagious) where

import BootstrapPercolation
import GraphExt

import Data.Graph.Types
import Data.Graph.UGraph

import Data.Maybe (fromMaybe, isNothing, fromJust)
import Data.List (delete)

import qualified Data.HashMap.Strict as HM

import Control.Monad.Identity
import qualified Control.Monad.Memo as Memo

import Data.Hashable

type MemoMin = Memo.MemoStateT (HM.HashMap (UGraph Int (), HM.HashMap Int Int, [Int]) [Int]) (UGraph Int (), HM.HashMap Int Int, [Int]) [Int]
type MemoMinV = Memo.MemoStateT (HM.HashMap (UGraph Int (), HM.HashMap Int Int, [Int], Int) (Maybe [Int])) (UGraph Int (), HM.HashMap Int Int, [Int], Int) (Maybe [Int])
type MemoRes = Memo.MemoStateT (HM.HashMap (UGraph Int (), HM.HashMap Int Int, [Int]) (Maybe [Int])) (UGraph Int (), HM.HashMap Int Int, [Int]) (Maybe [Int])

type MemoMR = MemoMin (MemoMinV (MemoRes Identity))

instance (Hashable v, Eq v, Hashable e) => Hashable (UGraph v e) where
  hashWithSalt s g = s `hashWithSalt` vertices g `hashWithSalt` edgeTriples g

instance (Eq k, Hashable k) => Memo.MapLike (HM.HashMap k v) k v where
  lookup = HM.lookup
  add = HM.insert

minimumContagious :: UGraph Int () -> Int -> [Int]
minimumContagious graph k = runIdentity $ Memo.evalMemoStateT (Memo.evalMemoStateT (Memo.evalMemoStateT (minimumContagiousM graph k) HM.empty) HM.empty) HM.empty

{-|
 - Find a minimal contagious set by our recursive algorithm, given a constant threshold.
 -}
minimumContagiousM :: UGraph Int () -> Int -> MemoMR [Int]
minimumContagiousM graph k = minimumContagiousSet graph thresholds
  where
    thresholds = foldr (`HM.insert` k) HM.empty $ vertices graph

{-|
 - Find a minimal contagious set by our recursive algorithm, given a threshold
 - function.
 -}
minimumContagiousSet :: UGraph Int () -> HM.HashMap Int Int -> MemoMR [Int]
minimumContagiousSet graph thresholds = Memo.for3 Memo.memol0 minimumContagiousSetItr graph updatedThresholds requiredVertices
  where
    requiredVertices = filter (\v -> vertexDegree graph v < fromJust (HM.lookup v thresholds)) $ vertices graph
    updatedThresholds = foldr (HM.adjust (const 0)) thresholds requiredVertices

minimumContagiousSetItr :: UGraph Int () -> HM.HashMap Int Int -> [Int] -> MemoMR [Int]
minimumContagiousSetItr graph thresholds requiredVertices =
  if not $ null possible then Memo.for3 Memo.memol0 minimumContagiousSetItr nextGraph nextThresholds requiredVertices
  else do
    restrictedSet <- Memo.for3 Memo.memol2 restrictedContagiousSet graph thresholds $ vertices graph
    return $ requiredVertices ++ fromMaybe (vertices graph) restrictedSet
  where
    possible = filter (\v -> fromJust (HM.lookup v thresholds) <= 0) $ vertices graph
    v = head possible
    nextGraph = removeVertex v graph
    nextThresholds = HM.delete v . foldr (HM.adjust (subtract 1)) thresholds $ adjacentVertices graph v -- "Fine, we'll keep that" - Henry, 11 October 2020 21:25

restrictedContagiousSet :: UGraph Int () -> HM.HashMap Int Int -> [Int] -> MemoMR (Maybe [Int])
restrictedContagiousSet graph thresholds considerable
  | null possible && length percConsiderable == order graph = return $ Just considerable
  | null possible = return Nothing
  | otherwise = do
      setWithV <- minimumContagiousSetWithV graph thresholds considerable v
      if isNothing setWithV then return Nothing
      else do
        nextCSet <- Memo.for3 Memo.memol2 restrictedContagiousSet graph thresholds $ delete v considerable
        if isNothing nextCSet || length (fromJust setWithV) < length (fromJust nextCSet) then return setWithV
        else return nextCSet
  where
    possible = filter (\v -> vertexDegree graph v >= fromJust (HM.lookup v thresholds)) considerable
    percConsiderable = percFunc graph thresholds considerable
    v = head possible

minimumContagiousSetWithVItr :: UGraph Int () -> HM.HashMap Int Int -> [Int] -> Int -> MemoMR (Maybe [Int])
minimumContagiousSetWithVItr graph thresholds considerable v =
  if not $ null possible then minimumContagiousSetWithVItr newGraph newThresholds newConsiderable v
  else do
    nextCSet <- Memo.for3 Memo.memol2 restrictedContagiousSet graph thresholds considerable
    if isNothing nextCSet then return Nothing
    else return $ Just (v : fromJust nextCSet)
  where
    possible = filter (\u -> fromJust (HM.lookup u thresholds) <= 0) $ vertices graph
    u = head possible
    thresholds1 = foldr (HM.adjust (\kw -> kw - 1)) thresholds $ adjacentVertices graph u
    newGraph = removeVertex u graph
    newThresholds = HM.delete u thresholds1
    newConsiderable = delete u considerable

minimumContagiousSetWithV :: UGraph Int () -> HM.HashMap Int Int -> [Int] -> Int -> MemoMR (Maybe [Int])
minimumContagiousSetWithV graph thresholds considered v = minimumContagiousSetWithVItr graph updatedThresholds considered v
  where
    updatedThresholds = HM.adjust (const 0) v thresholds
