module Block3.Task2Spec
  ( spec
  ) where

import Test.Hspec (Spec, shouldBe, describe, it)
import Block3.Task2 (ok, satisfy, element, eof, stream)
import Block3.Task1 (runP)

spec :: Spec
spec = do
  describe "combinators" $ do
    it "ok test" $ do
      runP ok "12" `shouldBe` Just ((), "12")
      runP ok [1 :: Int ,2 ,3] `shouldBe` Just ((), [1, 2, 3])
      runP ok [[1 :: Int] ,[2] ,[3]] `shouldBe` Just ((), ([[1] ,[2] ,[3]]))
      runP ok [["12"] ,["2"] ,["3"]]
       `shouldBe` Just ((), ([["12"] ,["2"] ,["3"]]))

    it "eof test" $ do
      runP eof "" `shouldBe` Just ((), [])
      runP eof [1 :: Int] `shouldBe` Nothing
      runP eof ["12"] `shouldBe` Nothing
      runP eof "21" `shouldBe` Nothing

    it "satisfy test" $ do
      runP (satisfy (== '1')) "123" `shouldBe` Just ('1', "23")
      runP (satisfy (== (1 :: Int))) [1, 2, 3] `shouldBe` Just (1, [2, 3])
      runP (satisfy (\s -> s >= '0' && s <= '9')) "256"
        `shouldBe` Just ('2', "56")
      runP (satisfy (\s -> s >= '0' && s <= '9')) "as" `shouldBe` Nothing

    it "element test" $ do
      runP (element '1') "123" `shouldBe` Just ('1', "23")
      runP (element (1 :: Int)) [1, 3, 4] `shouldBe` Just (1, [3, 4])
      runP (element ("12")) ["12", "13"] `shouldBe` Just ("12", ["13"])
      runP (element '2') "1221" `shouldBe` Nothing

    it "stream test" $ do
      runP (stream "12") "123" `shouldBe` Just ("12", "3")
      runP (stream [1 :: Int, 2]) [1, 2, 3, 4]
        `shouldBe` Just ([1, 2], [3, 4])
      runP (stream ["12", "123"]) ["12" ,"123", "322"]
        `shouldBe` Just(["12", "123"], ["322"])
      runP (stream ["12", "123"]) ["12" ,"1223", "322"] `shouldBe` Nothing
