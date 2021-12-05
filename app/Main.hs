module Main where

import System.IO
import Day01

main :: IO ()
main = putStr "this function is useless, sorry (:"

advent_01_part_1 :: IO Int 
advent_01_part_1 = do 
    contents <- readFile "app/input_files/advent_of_code_01.txt"
    return $ solveDay01part01 contents
advent_01_part_2 :: IO Int
advent_01_part_2 = do 
    contents <- readFile "app/input_files/advent_of_code_01.txt"
    return $ solveDay01part02 contents