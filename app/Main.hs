import Data.Graph.Types
import Data.Graph.Generation

import Control.Parallel.Strategies

import Control.Monad (replicateM)
import System.Environment

import BootstrapPercolation
import ReichmanAlgo
import MinimalContagiousMemo

main = do
  args <- getArgs
  let n = read $ head args
  let p = read $ args !! 1
  let k = read $ args !! 2
  let times = read $ args !! 3
  graphs <- replicateM times (erdosRenyiU n p)
  let results = withStrategy (parListChunk 128 rdeepseq) $ map (\g -> (g, minimumContagious g k, reichmanAlgo g k, reichmanBound g k, henryBound g k, henryBound2 g k, henryBound3 g k)) graphs
  putStrLn "g\tlen(V)\tlen(E)\topt\tlen(opt)\treichman\tlen(reichman)\tdbd\thbd\thbd2\thbd3"
  mapM_ (\(g, minSet, reichmanSet, dbd, hbd, hbd2, hbd3) -> do
    putStr $ show g
    putChar '\t'
    putStr . show $ order g
    putChar '\t'
    putStr . show $ size g
    putChar '\t'
    putStr $ show minSet
    putChar '\t'
    putStr . show $ length minSet
    putChar '\t'
    putStr $ show reichmanSet
    putChar '\t'
    putStr . show $ length reichmanSet
    putChar '\t'
    putStr $ show dbd
    putChar '\t'
    putStr $ show hbd
    putChar '\t'
    putStr $ show hbd2
    putChar '\t'
    putStr $ show hbd3
    putChar '\n') results
