module Block2.Task1Spec
  ( spec
  ) where

import Test.Hspec (Spec, shouldBe, describe, it)
import Block2.Task1 (eval, ArithmeticError(..), Expr(..))

spec :: Spec
spec = do
  describe "eval" $ do
    it "constant test" $ do
      eval (Cons 1) `shouldBe` Right (1 :: Int)
      eval (Cons 123) `shouldBe` Right (123 :: Int)
      eval (Cons (-234234)) `shouldBe` Right ((-234234) :: Int)
      eval (Cons 89548459) `shouldBe` Right (89548459 :: Int)

    it "add test" $ do
      eval (Add (Cons 1) (Cons 2)) `shouldBe` Right (3 :: Int)
      eval (Add (Cons 15) (Cons 20)) `shouldBe` Right (35 :: Int)
      eval (Add ((Add (Cons 15) (Cons 20))) ((Add (Cons 1) (Cons 2))))
        `shouldBe` Right (38 :: Int)
      eval (Add ((Add (Cons 15) (Cons 20))) (Cons 100))
        `shouldBe` Right (135 :: Int)

    it "sub test" $ do
      eval (Sub (Cons 2) (Cons 1)) `shouldBe` Right (1 :: Int)
      eval (Sub (Cons 15) (Cons 20)) `shouldBe` Right ((-5) :: Int)
      eval (Sub ((Sub (Cons 15) (Cons 20))) ((Sub (Cons 1) (Cons 2))))
        `shouldBe` Right ((-4) :: Int)
      eval (Sub ((Sub (Cons 15) (Cons 20))) (Cons (-100)))
        `shouldBe` Right (95 :: Int)

    it "mul test" $ do
      eval (Mul (Cons 2) (Cons 1)) `shouldBe` Right (2 :: Int)
      eval (Mul (Cons 15) (Cons 20)) `shouldBe` Right (300 :: Int)
      eval (Mul ((Mul (Cons 15) (Cons 20))) ((Mul (Cons 1) (Cons 2))))
        `shouldBe` Right (600 :: Int)
      eval (Mul ((Mul (Cons 15) (Cons 20))) (Cons (-10)))
        `shouldBe` Right ((-3000) :: Int)

    it "div test" $ do
      eval (Div (Cons 2) (Cons 1)) `shouldBe` Right (2 :: Int)
      eval (Div (Cons 15) (Cons 20)) `shouldBe` Right (0 :: Int)
      eval (Div ((Div (Cons 30) (Cons 2))) ((Div (Cons 6) (Cons 2))))
        `shouldBe` Right (5 :: Int)
      eval (Div (Div (Cons 300) (Cons 20)) (Cons (-3)))
        `shouldBe` Right (-5 :: Int)

    it "pow test" $ do
      eval (Pow (Cons 2) (Cons 1)) `shouldBe` Right (2 :: Int)
      eval (Pow (Cons 2) (Cons 20)) `shouldBe` Right (1048576 :: Int)
      eval (Pow ((Pow (Cons 30) (Cons 2))) ((Pow (Cons 1) (Cons 2))))
        `shouldBe` Right (900 :: Int)
      eval (Pow (Cons (-10)) (Cons 3)) `shouldBe` Right ((-1000) :: Int)

    it "failure test" $ do
      eval (Pow (Cons 2) (Cons (-1))) `shouldBe` Left NegateExponent
      eval (Pow (Cons 2) (Div (Cons (-15)) (Cons 4)))
        `shouldBe` Left NegateExponent
      eval (Div (Cons 1454) (Cons 0)) `shouldBe` Left DivisionByZero
      eval (Div (Cons 1454) (Sub (Cons 1) (Cons 1)))
        `shouldBe` Left DivisionByZero

    it "random test" $ do
      eval
        (Add (Mul (Add (Cons 12) (Cons 10)) (Sub (Cons 45) (Cons 54))) (Cons 100))
        `shouldBe` Right ((-98) :: Int)
      eval (Pow
        (Mul (Div (Cons 12) (Cons 5)) (Sub (Cons 45) (Cons 54)))
        (Add (Cons 1) (Cons 2)))
        `shouldBe` Right ((-5832) :: Int)
