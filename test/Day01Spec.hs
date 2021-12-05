import Test.Hspec
import Day01

main :: IO ()
main = hspec $ do
  describe "countIncreasesOnList" $ do
    it "should return 0 for single entry" $ do
      countIncreasesOnList [1] `shouldBe` (0 :: Int)

    it "should return 1 for single incrementing" $ do
      countIncreasesOnList [1,2] `shouldBe` (1 :: Int)
    
    it "should return 0 for not incrementing" $ do
      countIncreasesOnList [1,1,1] `shouldBe` (0 :: Int)     
      
    it "should pass on spec" $ do
      countIncreasesOnList [199,200,208,210,200,207,240,269,260,263] `shouldBe` (7 :: Int)    

  describe "formatFileContentAsIntegerList" $ do
      it "should work" $ do
        formatFileContentAsIntegerList "1\n2\n3" `shouldBe` ([1,2,3])

  describe "countIncreaseOnSlidingWindow" $ do
    it "should work for spec" $ do
      countIncreaseOnSlidingWindow [199,200,208,210,200,207,240,269,260,263] `shouldBe` (5 :: Int)
      

      
