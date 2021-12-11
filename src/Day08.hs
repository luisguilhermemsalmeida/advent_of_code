module Day08 (
    solveDay08part01,
    -- solveDay08part02,
) where

import Data.Function
import Data.List
import Data.Ord
import Utils
import Data.List.Split

solveDay08part01 :: String -> Int
solveDay08part01 fileContents = fileContents 
    & lines
    & map (splitOn "|")
    & map (\line -> line !! 1)
    & map (splitOn " ")
    & concat
    & filter (\v -> length v `elem` [2, 3, 4, 7])
    & length

solveDay08part02 :: String -> Int
solveDay08part02 fileContents = fileContents 
    & lines
    & map (splitOn "|")
    & map (\line -> line !! 1)
    & map (splitOn " ")
    & concat
    & filter (\v -> length v `elem` [2, 3, 4, 7])
    & length
    