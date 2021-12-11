module Day04Spec where

import Test.Hspec
import Day04

input :: String
input = unlines [
  "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1",
  "",
  "22 13 17 11  0",
  "8  2 23  4 24",
  "21  9 14 16  7",
  "6 10  3 18  5",
  "1 12 20 15 19",
  "",
  "3 15  0  2 22",
  "9 18 13 17  5",
  "19  8  7 25 23",
  "20 11 10 24  4",
  "14 21 16 12  6",
  "",
  "14 21 17 24  4",
  "10 16 15  9 19",
  "18  8 23 26 20",
  "22 11 13  6  5",
  "2  0 12  3  7"
  ]

simpleInput = unlines [
  "1,2,3,4,5,6",
  "",
  "1  2",
  "3  4",
  "5  6",
  "7  8",
  "9 10",
  "",
  "1 2",
  "3 4",
  "5 6",
  "7 8",
  "9 10"
  ]


spec :: Spec
spec = do
  describe "solveDay04part01" $ do
    it "should pass spec" $ do
      solveDay04part01 simpleInput `shouldBe` (104 :: Int)
      solveDay04part01 input `shouldBe` (4512 :: Int)

  describe "solveDay04part02" $ do
    it "should pass spec" $ do
      solveDay04part02 input `shouldBe` (1924 :: Int)

  describe "board has winning condiction" $ do
    it "should work" $ do
      boardHasWinningCondition 
        [3, 2, 1]
        ([[1,2],[3,4],[5,6],[7,8],[9,10]],[[1,3,5,7,9],[2,4,6,8,10]]) 
      `shouldBe`
        True

  describe "input reader" $ do
    it "should read correctly" $ do
      parseInput simpleInput `shouldBe` (
        [1,2,3,4,5,6],
        [
          (
            [[1, 2], [3, 4], [5, 6], [7, 8], [9, 10]],
            [[1, 3, 5, 7, 9], [2, 4, 6, 8, 10]]
          ),
          (
            [[1, 2], [3, 4], [5, 6], [7, 8], [9, 10]],
            [[1, 3, 5, 7, 9], [2, 4, 6, 8, 10]]
          )
        ]
        )
  -- describe "solveDay08part02" $ do
  --   it "should pass spec" $ do
  --     solveDay07part02 input `shouldBe` (168 :: Int)
  
