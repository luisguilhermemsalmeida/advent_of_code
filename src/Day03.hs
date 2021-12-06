module Day03 (
    solveDay03part01,
    solveDay03part02,
) where

import Data.Function
import Data.List

import Utils
import Day01 (formatFileContentAsIntegerList)

solveDay03part01 :: String -> Int ->  Int
solveDay03part01 fileContents inputSize = fileContents
                                & lines
                                & transpose
                                & map mostCommonElementOnList
                                & binaryStringToDecimal
                                & multiplyWithCompliment inputSize

solveDay03part02 :: String ->  Int
solveDay03part02 fileContents = calculateRatings (lines fileContents)

calculateRatings :: [String] -> Int
calculateRatings lines = calculateOxygenGeneratorRating lines * calculateCO2ScrubberRating lines

calculateOxygenGeneratorRating :: [String] -> Int
calculateOxygenGeneratorRating lines = lines 
    & filterByMostCommonDigit 0 (/=)
    & binaryStringToDecimal


calculateCO2ScrubberRating :: [String] -> Int
calculateCO2ScrubberRating lines = lines 
    & filterByMostCommonDigit 0 (==)
    & binaryStringToDecimal



multiplyWithCompliment :: Int -> Int -> Int
multiplyWithCompliment inputSize value = value * (2 ^ inputSize - 1 - value)

withTranspose :: [String] -> ([String], [String])
withTranspose x = (x, transpose x)

filterByMostCommonDigit :: Int -> (Char -> Char -> Bool) -> [String] -> String 
filterByMostCommonDigit step operator [line]  = line
filterByMostCommonDigit step operator lines = filterByMostCommonDigit (step + 1) operator (filter (\line -> line !! step `operator` mostCommonElementOnList (transpose lines !! step)) lines) 

