module Day07 (
    solveDay07part01,
    solveDay07part02,
) where

import Data.Function
import Data.List
import Data.Ord
import Utils
import Data.List.Split

solveDay07part01 :: String -> Int
solveDay07part01 fileContents = fileContents 
    & splitOn ","
    & map (\v -> read v :: Int)
    & produceCostList errorFunction
    & minWithIndex
    & getFuel
    
solveDay07part02 :: String -> Int
solveDay07part02 fileContents = fileContents 
    & splitOn ","
    & map (\v -> read v :: Int)
    & produceCostList errorFunctionProgressive
    & minWithIndex
    & getFuel

getFuel :: (Int, Int) -> Int
getFuel (fuel, _index) = fuel

errorFunction :: Num a => a -> [a] -> a
errorFunction value list = sum $ map (\v -> abs(v - value)) list

errorFunctionProgressive :: Int -> [Int] -> Int
errorFunctionProgressive value list = sum $ map (\v -> (abs(v - value) * (abs(v - value) + 1)) `div` 2 ) list

minWithIndex :: [Int] -> (Int, Int)
minWithIndex xs = minimumBy (comparing fst) (zip xs [0..])

produceCostList :: (Int -> [Int] -> Int) -> [Int] -> [Int]
produceCostList errorFunction inputList = [ errorFunction i inputList | i <- [0..listMaximum inputList]]
