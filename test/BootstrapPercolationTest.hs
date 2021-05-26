module Main where

import Test.HUnit

import Data.Graph.Types

import BootstrapPercolation

twoVertexPath = insertEdgePair (0, 1) empty

threeVertexPath = insertEdgePairs [(0, 1), (1, 2)] empty

kleinFour = insertEdgePairs [(0, 1), (0, 2), (1, 2), (1, 3), (2, 3)] empty
kleinFourAlt = insertEdgePairs [(0, 1), (0, 3), (1, 3), (1, 2), (3, 2)] empty

twoVertexThresholdOne = perc twoVertexPath 1 [0] ~?= [0, 1]

threeVertexThresholdOne = perc threeVertexPath 1 [0, 2] ~?= [0, 1, 2]

threeVertexThresholdTwo = perc threeVertexPath 2 [0, 2] ~?= [0, 1, 2]

threeVertexThresholdThree = perc threeVertexPath 3 [0, 2] ~?= [0, 2]

kleinFourThresholdTwo = perc kleinFour 2 [0, 1] ~?= [0, 1, 2, 3]
kleinFourAltThresholdTwo = perc kleinFourAlt 2 [0, 1] ~?= [0, 1, 2, 3]

main = runTestTTAndExit $ TestList [ twoVertexThresholdOne
                                   , threeVertexThresholdOne 
                                   , threeVertexThresholdTwo
                                   , threeVertexThresholdThree
                                   , kleinFourThresholdTwo
                                   , kleinFourAltThresholdTwo
                                   ]
