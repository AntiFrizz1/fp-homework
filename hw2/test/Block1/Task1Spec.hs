module Block1.Task1Spec
  ( spec
  ) where

import Test.Hspec (Spec, shouldBe, describe, it)
import Block1.Task1 (stringSum)

spec :: Spec
spec = do
  describe "stringSum" $ do
    it "simple tests" $ do
      stringSum "1 2 3 4 5" `shouldBe` Just (15 :: Int)
      stringSum "1 1 1 1 1" `shouldBe` Just (5 :: Int)
      stringSum "105 101 120" `shouldBe` Just (326 :: Int)

    it "different whitespaces" $ do
      stringSum "        1     2    3    4    5" `shouldBe` Just (15 :: Int)
      stringSum "\n10\n20\n30\n\n\n\n40\n\n\n\n50" `shouldBe` Just (150 :: Int)
      stringSum "10000\t\t2000\t300\t\t40\t\t5\t" `shouldBe` Just (12345 :: Int)
      stringSum "10000\r\r2000\r300\r\r40\r\r5\r" `shouldBe` Just (12345 :: Int)
      stringSum " 10\t\n2000\r30000  \r4\n\r50  " `shouldBe` Just (32064 :: Int)

    it "big tests" $ do
      stringSum "1000  322 123   15818 168 51   " `shouldBe` Just (17482 :: Int)
      stringSum "100000000 12 234234 2342" `shouldBe` Just (100236588 :: Int)

    it "failure tests" $ do
      stringSum "1 2 !3 4 5" `shouldBe` Nothing
      stringSum "1 [2  `` 3 4 5" `shouldBe` Nothing
      stringSum "1 C2 !3 4 5" `shouldBe` Nothing
      stringSum "1 2 3 4Q 5" `shouldBe` Nothing
