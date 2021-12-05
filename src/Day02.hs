module Day02 (
    solveDay02part01,
    solveDay02part02,
    formatFileContentAsInstructionList,
    Instruction(..)
) where

import Data.Function
import Utils

data Instruction = Forward | Up | Down deriving (Show, Eq)

solveDay02part01 :: String -> Int
solveDay02part01 fileContents = fileContents
                                & formatFileContentAsInstructionList 
                                & processInstructions
                                & multiplyDepthAndHorizontalPosition
solveDay02part02 :: String -> Int
solveDay02part02 fileContents = fileContents
                                & formatFileContentAsInstructionList 
                                & processInstructionsWithAim
                                & multiplyDepthAndHorizontalPosition



formatFileContentAsInstructionList :: String -> [(Instruction, Int)]
formatFileContentAsInstructionList fileContent = fileContent
                                                 & lines
                                                 & map words
                                                 & map (\[instruction, number] -> (instruction, number))
                                                 & map (\(instruction, number) -> (stringToInstruction instruction, read number :: Int))

stringToInstruction :: String -> Instruction
stringToInstruction "forward" = Forward
stringToInstruction "up" = Up
stringToInstruction "down" = Down
stringToInstruction _ = error "Failed"

processInstructions :: [(Instruction, Int)] -> (Int, Int)
processInstructions = foldl accumulateInstructions (0, 0)

accumulateInstructions :: (Int, Int) -> (Instruction, Int) -> (Int, Int)
accumulateInstructions (depth, horizontalPosition) (Forward, number)  = (depth, horizontalPosition + number) 
accumulateInstructions (depth, horizontalPosition) (Up, number)  = (depth - number, horizontalPosition) 
accumulateInstructions (depth, horizontalPosition) (Down, number)  = (depth + number, horizontalPosition) 

processInstructionsWithAim :: [(Instruction, Int)] -> (Int, Int)
processInstructionsWithAim x = tripleToTuple $ foldl accumulateInstructionsWithAim (0, 0, 0) x

tripleToTuple :: (Int, Int, Int) -> (Int, Int)
tripleToTuple (a, b, _) = (a, b)

accumulateInstructionsWithAim :: (Int, Int, Int) -> (Instruction, Int) -> (Int, Int, Int)
accumulateInstructionsWithAim (depth, horizontalPosition, aim) (Forward, number)  = (depth + aim * number, horizontalPosition + number, aim) 
accumulateInstructionsWithAim (depth, horizontalPosition, aim) (Up, number)  = (depth, horizontalPosition, aim - number) 
accumulateInstructionsWithAim (depth, horizontalPosition, aim) (Down, number)  = (depth, horizontalPosition, aim + number) 

multiplyDepthAndHorizontalPosition :: (Int, Int) -> Int
multiplyDepthAndHorizontalPosition (depth, horizontalPosition) = depth * horizontalPosition