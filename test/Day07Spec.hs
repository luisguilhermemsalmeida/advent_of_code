module Day07Spec where

import Test.Hspec
import Day07

input :: String
input = "16,1,2,0,4,2,7,1,2,14"

spec :: Spec
spec = do
  describe "solveDay07part01" $ do
    it "should pass spec" $ do
      solveDay07part01 input `shouldBe` (37 :: Int)
  describe "solveDay07part02" $ do
    it "should pass spec" $ do
      solveDay07part02 input `shouldBe` (168 :: Int)
  
