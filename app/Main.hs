module Main where

import System.IO
import Day01
import Day02
import Day03
import Day04
import Day07
import Day08

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

advent_04_part_1 :: IO Int 
advent_04_part_1 = do 
    contents <- readFile "app/input_files/advent_of_code_04.txt"
    return $ solveDay04part01 contents
    
advent_04_part_2 :: IO Int 
advent_04_part_2 = do 
    contents <- readFile "app/input_files/advent_of_code_04.txt"
    return $ solveDay04part02 contents

advent_07_part_1 :: IO Int 
advent_07_part_1 = do 
    contents <- readFile "app/input_files/advent_of_code_07.txt"
    return $ solveDay07part01 contents
advent_07_part_2 :: IO Int 
advent_07_part_2 = do 
    contents <- readFile "app/input_files/advent_of_code_07.txt"
    return $ solveDay07part02 contents

advent_08_part_1 :: IO Int 
advent_08_part_1 = do 
    contents <- readFile "app/input_files/advent_of_code_08.txt"
    return $ solveDay08part01 contents
    
-- advent_08_part_2 :: IO Int 
-- advent_08_part_2 = do 
--     contents <- readFile "app/input_files/advent_of_code_08.txt"
--     return $ solveDay08part02 contents
