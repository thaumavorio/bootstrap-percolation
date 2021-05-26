{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import ReichmanAlgo
import MinimalContagious

import Data.Graph.Types
import Data.Graph.UGraph

import Web.Scotty

import GHC.Generics

import qualified Data.Aeson as A

data YAG v = YAG { vertices :: [v], edges :: [(v,v)] }
  deriving (Show, Generic)
data YARF v = YARF { graph :: YAG v, threshold :: Int }
  deriving (Show, Generic)

instance A.FromJSON v => A.FromJSON (YAG v)
instance A.FromJSON v => A.FromJSON (YARF v)

buildGraphYARF (YARF (YAG vs es) _) = insertEdgePairs es $ insertVertices vs empty

main :: IO ()
main = scotty 8081 $ do
  post "/greedy" $ do
    reqData <- jsonData
    let receivedGraph = buildGraphYARF reqData :: UGraph Int ()
    json $ reichmanAlgo receivedGraph $ threshold reqData
  post "/min" $ do
    reqData <- jsonData
    let receivedGraph = buildGraphYARF reqData :: UGraph Int ()
    json $ minimumContagious receivedGraph $ threshold reqData
