module Day02Spec where

import Test.Hspec
import Day02

input :: String
input = "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2"

spec :: Spec
spec = do
  describe "formatFileContentAsInstructionList" $ do
      it "should transform input" $ do
        formatFileContentAsInstructionList input `shouldBe` [(Forward, 5), (Down, 5), (Forward, 8), (Up, 3), (Down, 8), (Forward, 2)]
  describe "solveDay02part01" $ do
    it "should pass spec" $ do
      solveDay02part01 input `shouldBe` (150 :: Int)
  describe "solveDay02part02" $ do
    it "should pass spec" $ do
      solveDay02part02 input `shouldBe` (900 :: Int)
  
