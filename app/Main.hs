module Main where

import System.IO
import Day01
import Day02
import Day03

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

advent_02_part_1 :: IO Int 
advent_02_part_1 = do 
    contents <- readFile "app/input_files/advent_of_code_02.txt"
    return $ solveDay02part01 contents

advent_02_part_2 :: IO Int 
advent_02_part_2 = do 
    contents <- readFile "app/input_files/advent_of_code_02.txt"
    return $ solveDay02part02 contents

advent_03_part_1 :: IO Int 
advent_03_part_1 = do 
    contents <- readFile "app/input_files/advent_of_code_03.txt"
    return $ solveDay03part01 contents 12
advent_03_part_2 :: IO Int 
advent_03_part_2 = do 
    contents <- readFile "app/input_files/advent_of_code_03.txt"
    return $ solveDay03part02 contents
