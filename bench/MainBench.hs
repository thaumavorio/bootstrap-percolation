import Criterion.Main

import Data.Graph.Types
import Data.Graph.UGraph
import Data.Graph.Generation

import GraphExt
import qualified MinimalContagious as M
import qualified MinimalContagiousMemo as MMemo

buildBGroup :: (UGraph Int () -> Int -> [Int]) -> Int -> Int -> [Float] -> [Benchmark]
buildBGroup f k n = map (\p -> bench ("G(" ++ show n ++ ", " ++ show p ++ ")") $ nfIO $ erdosRenyiU n p >>= (\g -> pure $ f g k))

n = 20
k = 2
ps = [0.1, 0.09, 0.08, 0.07, 0.06, 0.05]

main :: IO ()
main = defaultMain [-- bgroup "bf k=2" $ buildBGroup M.minimalContagious k n ps
                     bgroup "rec k=2" $ buildBGroup M.minimumContagious k n ps
                   , bgroup "rec-memo k=2" $ buildBGroup MMemo.minimumContagious k n ps
                   ]
