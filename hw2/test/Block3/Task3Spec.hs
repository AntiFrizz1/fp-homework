module Block3.Task3Spec
  ( spec
  ) where

import Test.Hspec (Spec, shouldBe, describe, it)
import Block3.Task3 (parseBrackets, parseNumber)
import Block3.Task1 (runP)

spec :: Spec
spec = do
  describe "brackets parser" $ do
    it "correct brackets" $ do
      runP parseBrackets "()()()()()()" `shouldBe` Just ((), [])
      runP parseBrackets "((((()))))" `shouldBe` Just ((), [])
      runP parseBrackets "((()((()))()()))" `shouldBe` Just ((), [])

    it "failure brackets" $ do
      runP parseBrackets "))((())))))" `shouldBe` Nothing
      runP parseBrackets "((((()" `shouldBe` Nothing
      runP parseBrackets "((((()))(()))" `shouldBe` Nothing

    it "brackets with trash" $ do
      runP parseBrackets "(()(1))" `shouldBe` Nothing
      runP parseBrackets "()()1" `shouldBe` Nothing
      runP parseBrackets " 1()()()" `shouldBe` Nothing

  describe "number parser" $ do
    it "simple numbers" $ do
      runP parseNumber "1213" `shouldBe` Just (1213 :: Int, [])
      runP parseNumber "1337" `shouldBe` Just (1337 :: Int, [])
      runP parseNumber "1" `shouldBe` Just (1 :: Int, [])
      runP parseNumber "2" `shouldBe` Just (2 :: Int, [])

    it "numbers with plus" $ do
      runP parseNumber "+1213" `shouldBe` Just (1213 :: Int, [])
      runP parseNumber "+1337" `shouldBe` Just (1337 :: Int, [])
      runP parseNumber "+1" `shouldBe` Just (1 :: Int, [])
      runP parseNumber "+2" `shouldBe` Just (2 :: Int, [])

    it "numbers with minus" $ do
      runP parseNumber "-1213" `shouldBe` Just ((-1213) :: Int, [])
      runP parseNumber "-1337" `shouldBe` Just ((-1337) :: Int, [])
      runP parseNumber "-1" `shouldBe` Just ((-1) :: Int, [])
      runP parseNumber "-2" `shouldBe` Just ((-2) :: Int, [])

    it "failure numbers" $ do
      runP parseNumber "" `shouldBe` Nothing
      runP parseNumber "--1337" `shouldBe` Nothing
      runP parseNumber "++1" `shouldBe` Nothing
      runP parseNumber "  12" `shouldBe` Nothing
