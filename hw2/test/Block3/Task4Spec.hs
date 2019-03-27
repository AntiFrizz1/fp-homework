module Block3.Task4Spec
  ( spec
  ) where

import Test.Hspec (Spec, shouldBe, describe, it)
import Block3.Task4 (listlistParser)
import Block3.Task1 (runP)
spec :: Spec
spec = do
  describe "listlistParser" $ do
    it "simple tests" $ do
      runP listlistParser "1, 2, 3, 1, 2, 3"
        `shouldBe` Just ([[2 :: Int], [1, 2, 3]], "")
      runP listlistParser "0, 2, 3, 1, 1, 3"
        `shouldBe` Just ([[], [3 :: Int, 1], [3]], "")
      runP listlistParser "0, 1, 3, 2, 1, 3"
        `shouldBe` Just ([[], [3 :: Int], [1, 3]], "")

    it "empty test" $ do
      runP listlistParser "" `shouldBe` Just ([], "")

    it "zero test" $ do
      runP listlistParser "0, 0, 0, 0, 0"
        `shouldBe` Just ([[], [], [], [], []], "")

    it "sign test" $ do
      runP listlistParser "2, +1, -1, 3, 12, +112, 1"
        `shouldBe` Just ([[1 :: Int, -1], [12, 112, 1]], "")
      runP listlistParser "1, +1, 1, -3, 1, 256"
        `shouldBe` Just ([[1 :: Int], [-3], [256]], "")

    it "whitespace test" $ do
      runP listlistParser "  2 ,  \t\n 1 , \r 1, 0    "
        `shouldBe` Just([[1 :: Int, 1], []], "")
      runP listlistParser "\n\t  3 ,  \t\n 1 , \r 1, 0 \r   "
        `shouldBe` Just([[1 :: Int, 1, 0]], "")

    it "random test" $ do
      runP listlistParser "3, 2, 1,\n\r 4, 1, -3,2 ,+5,   21 "
        `shouldBe` Just ([[2 :: Int, 1, 4], [-3], [5, 21]], "")

    it "failure test" $ do
      runP listlistParser ",1" `shouldBe` Nothing
      runP listlistParser "1, 1," `shouldBe` Nothing
      runP listlistParser "1, q" `shouldBe` Nothing
      runP listlistParser "-1, 1" `shouldBe` Nothing
      runP listlistParser "1, -11, -2, 1, 2" `shouldBe` Nothing
