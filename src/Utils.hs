module Utils where

import Debug.Trace
import Data.List ( foldl, group, sort, maximumBy )
import Data.Function ( on, (&) )
import Data.Char (digitToInt)

mostCommonElementOnList :: Ord a => [a] -> a
mostCommonElementOnList = head . maximumBy (compare `on` length) . group . sort

binaryStringToDecimal :: String -> Int
binaryStringToDecimal = foldl (\acc x -> acc * 2 + digitToInt x) 0

display :: Show a => a -> a
display x = (trace $ show x) x

listMaximum :: [Int] -> Int
listMaximum = maximum 