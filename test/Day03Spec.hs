module Day03Spec where

import Test.Hspec
import Day03

input :: String
input = unlines [
  "00100",
  "11110",
  "10110",
  "10111",
  "10101",
  "01111",
  "00111",
  "11100",
  "10000",
  "11001",
  "00010",
  "01010"]

spec :: Spec
spec = do
  describe "solveDay02part01" $ do
    it "should pass spec" $ do
      solveDay03part01 input 5 `shouldBe` (198 :: Int)
  describe "solveDay03part02" $ do
    it "should pass spec" $ do
      solveDay03part02 input `shouldBe` (230 :: Int)
  
