module Main where

import Test.HUnit

import Data.Graph.Types

import Data.List (sort)

import MinimalContagious

twoVertexPath = insertEdgePair (0, 1) empty

threeVertexPath = insertEdgePairs [(0, 1), (1, 2)] empty

kleinFour = insertEdgePairs [(0, 1), (0, 2), (1, 2), (1, 3), (2, 3)] empty
kleinFourAlt = insertEdgePairs [(0, 1), (0, 3), (1, 3), (1, 2), (3, 2)] empty

twoVertexThresholdOne = minimumContagious twoVertexPath 1 `elem` [[0], [1], [2]] ~? "min of 2-vertex path; k = 1"

threeVertexThresholdOne = minimumContagious threeVertexPath 1 `elem` [[0], [1], [2]] ~? "min of 3-vertex path; k = 1"
threeVertexThresholdTwo = sort (minimumContagious threeVertexPath 2) `elem` [[0, 2]] ~? "min of 3-vertex path; k = 2"
threeVertexThresholdThree = sort (minimumContagious threeVertexPath 3) `elem` [[0, 1, 2]] ~? "min of 3-vertex path; k = 3"

kleinFourThresholdTwo = sort (minimumContagious kleinFour 2) `elem` [[0, 1], [0, 2], [1, 2], [1, 3], [2, 3]] ~? "min of klein four; k = 3"
kleinFourAltThresholdTwo = sort (minimumContagious kleinFourAlt 2) `elem` [[0, 1], [0, 3], [1, 2], [1, 3], [2, 3]] ~? "min of klein four (alt. indices); k = 3"

main = runTestTTAndExit $ TestList [ twoVertexThresholdOne
                                   , threeVertexThresholdOne 
                                   , threeVertexThresholdTwo
                                   , threeVertexThresholdThree
                                   , kleinFourThresholdTwo
                                   , kleinFourAltThresholdTwo
                                   ]
