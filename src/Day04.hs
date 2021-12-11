module Day04 (
    solveDay04part01,
    solveDay04part02,
    parseInput,
    boardHasWinningCondition
) where

import Data.Function
import Data.List

import Utils
import Data.List.Split (splitOn, chunksOf)

type Board = ([[Int]], [[Int]])
type Boards = [Board]
type Draws = [Int]

solveDay04part01 :: String ->  Int
solveDay04part01 fileContents = fileContents
    & parseInput
    & pullDrawsUntilWinner

solveDay04part02 :: String ->  Int
solveDay04part02 fileContents = fileContents
    & parseInput
    & mapNecessaryStepsForEachBoard

mapNecessaryStepsForEachBoard :: (Draws, Boards) -> Int
mapNecessaryStepsForEachBoard (draws, boards) = boards
    & map (necessaryStepForBoard draws)
    & maximumBy (\(_, a) (_, b) -> compare a b)
    & calculateScoreGivenBoardAndDraws draws

calculateScoreGivenBoardAndDraws :: Draws -> (Board, Int) -> Int
calculateScoreGivenBoardAndDraws draws (board, numberOfDraws) =
    calculateScore (take numberOfDraws draws)
    where calculateScore drawsUntilNow = board & findAllElementsNotDrawn drawsUntilNow & sum & (*last drawsUntilNow)

necessaryStepForBoard :: [Int] -> Board -> (Board, Int)
necessaryStepForBoard draws board = 
    (board, calculateEnoughDrawsForBoard board draws 1)

calculateEnoughDrawsForBoard :: Board -> [Int] -> Int -> Int
calculateEnoughDrawsForBoard board draws numberOfCalledNumbers =
    draws 
    & take numberOfCalledNumbers
    & flip boardHasWinningCondition board
    & hadWinningCondition
    where hadWinningCondition True = numberOfCalledNumbers
          hadWinningCondition False = calculateEnoughDrawsForBoard board draws (numberOfCalledNumbers + 1)

pullDrawsUntilWinner :: (Draws, Boards) -> Int
pullDrawsUntilWinner (draws, boards) = winningBoardOnCurrentDraw boards draws []
                                                     
winningBoardOnCurrentDraw :: Boards -> Draws -> Draws -> Int
winningBoardOnCurrentDraw boards [] _ = error "no one won"
winningBoardOnCurrentDraw boards (currentDraw:futureDraws) pastDraws = calculateScore $ findWinningBoard (currentDraw:pastDraws) boards
    where calculateScore Nothing = winningBoardOnCurrentDraw boards futureDraws (pastDraws ++ [currentDraw])
          calculateScore (Just board) = board & findAllElementsNotDrawn (pastDraws ++ [currentDraw]) & sum & (*currentDraw)

findAllElementsNotDrawn :: Draws -> Board -> [Int]
findAllElementsNotDrawn drawsUntilThisMoment (board, _) = board & concat & filter (`notElem` drawsUntilThisMoment)

findWinningBoard :: Draws -> Boards -> Maybe Board
findWinningBoard draws boards = boards 
    & filter (boardHasWinningCondition draws)
    & winningBoardOrNothing

winningBoardOrNothing :: [Board] -> Maybe Board
winningBoardOrNothing [] = Nothing
winningBoardOrNothing (head:_) = Just head

boardHasWinningCondition :: Draws -> Board -> Bool
boardHasWinningCondition draws (boardRows, boardColumns) = 
    (foldl (lineWasDrawn $ sort draws) False (boardRows & map sort)) || (foldl (lineWasDrawn $ sort draws) False (boardColumns & map sort))
    where lineWasDrawn draws acc line = acc || intersect line draws == line

parseInput :: String -> (Draws, Boards) 
parseInput fileContent = fileContent
    & lines
    & splitFirstWithRest
    & parseDraws
    & parseBoards

parseBoards (draw, boardLines) = boardLines
    & chunksOf 6
    & map (\(head:tail) -> tail)
    & readChunks
    & map (\board -> (board, transpose board))
    & (\board -> (draw, board))

readChunks :: [[String]] -> [[[Int]]]
readChunks chunks = chunks
    & map readChunk

readChunk :: [String] -> [[Int]]
readChunk chunk = chunk 
    & map (\line -> map (\v -> read v :: Int) . filter (/= "") . splitOn " " $ line)

parseDraws :: (String, [String])  -> (Draws, [String])
parseDraws (draw, boardLines) = (map (\v -> read v :: Int) $ splitOn "," draw, boardLines)

splitFirstWithRest :: [String] -> (String, [String])
splitFirstWithRest [] = error "invalid input"
splitFirstWithRest (first:rest) = (first, rest)
