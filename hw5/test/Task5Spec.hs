module Task5Spec
  ( spec
  ) where

import Test.Hspec (Spec, shouldBe, describe, it)
import Task5 (_1, _2)
import Lens.Micro (set, (^.), over)

spec :: Spec
spec = do
  describe "first" $ do
    it "set" $ do
      set _1 (4 :: Int) (2 :: Int, 3 :: Int) `shouldBe` (4 :: Int, 3 :: Int)
      set _1 "11" ("1", 3 :: Int) `shouldBe` ("11", 3 :: Int)
      set _1 (Just (12 :: Int)) (Nothing, 3 :: Int)
       `shouldBe` (Just (12 :: Int), (3 :: Int))

    it "view" $ do
      (1 :: Int, 2 :: Int) ^. _1 `shouldBe` (1 :: Int)
      ("11", 2 :: Int) ^. _1 `shouldBe` "11"
      (Nothing :: Maybe Int, 2 :: Int) ^. _1 `shouldBe` (Nothing :: Maybe Int)

    it "over" $ do
      over _1 (+ 1) (1 :: Int, 2 :: Int) `shouldBe` (2 :: Int, 2 :: Int)
      over _1 (fmap (+1)) (Just 12 :: Maybe Int, 2 :: Int)
       `shouldBe` (Just 13 :: Maybe Int, 2 :: Int)
      over _1 (const Nothing) (Just 12 :: Maybe Int, 2 :: Int)
       `shouldBe` (Nothing :: Maybe Int, 2 :: Int)

  describe "second" $ do
    it "set" $ do
      set _2 (4 :: Int) (2 :: Int, 4 :: Int) `shouldBe` (2 :: Int, 4 :: Int)
      set _2 "11" ("1", "33") `shouldBe` ("1", "11")
      set _2 (Just 12 :: Maybe Int) (Nothing :: Maybe Int, Nothing :: Maybe Int)
       `shouldBe` (Nothing :: Maybe Int, Just 12 :: Maybe Int)

    it "view" $ do
      (11 :: Int, 2 :: Int) ^. _2 `shouldBe` (2 :: Int)
      ("11", "122") ^. _2 `shouldBe` "122"
      (Nothing :: Maybe Int, Just 1 :: Maybe Int) ^. _2
       `shouldBe` (Just 1 :: Maybe Int)

    it "over" $ do
      over _2 (+ 1) (1 :: Int, 2 :: Int) `shouldBe` (1 :: Int, 3 :: Int)
      over _2 (fmap (+1)) (Just 12 :: Maybe Int, Just 2 :: Maybe Int)
       `shouldBe` (Just 12 :: Maybe Int, Just 3 :: Maybe Int)
      over _2 (const Nothing) (Just 12 :: Maybe Int, Just 2 :: Maybe Int)
       `shouldBe` (Just 12 :: Maybe Int, Nothing :: Maybe Int)
