module Day01 (
    solveDay01part01, solveDay01part02, countIncreasesOnList, formatFileContentAsIntegerList, countIncreaseOnSlidingWindow
) where
import Data.Function

solveDay01part01 :: String -> Int
solveDay01part01 fileContents = fileContents & formatFileContentAsIntegerList & countIncreasesOnList

solveDay01part02 :: String -> Int
solveDay01part02 = countIncreaseOnSlidingWindow . formatFileContentAsIntegerList 

formatFileContentAsIntegerList :: String -> [Int]
formatFileContentAsIntegerList content = map (\x -> read x :: Int) $ lines content

countIncreasesOnList :: [Int] -> Int
countIncreasesOnList x = accumulateIncreasesOnList x 9999

countIncreaseOnSlidingWindow :: [Int] -> Int
countIncreaseOnSlidingWindow x = compareTuples $ zip x ([9999,9999,9999] ++ x)

compareTuples :: [(Int, Int)] -> Int
compareTuples [] = 0
compareTuples ((x, y):tail)
    | x > y = 1 + compareTuples tail
    | otherwise = 0 + compareTuples tail

accumulateIncreasesOnList :: [Int] -> Int -> Int
accumulateIncreasesOnList [] _last_element = 0
accumulateIncreasesOnList (head:tail) last_element
    | last_element < head = 1 + accumulateIncreasesOnList tail head
    | otherwise = 0 + accumulateIncreasesOnList tail head


