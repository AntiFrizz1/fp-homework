{-# LANGUAGE BangPatterns #-}
module Task2
  ( plus
  , minus
  , scalarProduct
  , crossProduct
  , Point(Point)
  , perimeter
  , slowPerimeter
  , doubleArea
  , simpleDoubleArea
  ) where

import Data.List (foldl')
import Control.Parallel.Strategies (runEval, rpar, rseq, rdeepseq)

data Point = Point Int Int

plus :: Point -> Point -> Point
plus (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

minus :: Point -> Point -> Point
minus (Point x1 y1) (Point x2 y2) = Point (x1 - x2) (y1 - y2)

scalarProduct :: Point -> Point -> Int
scalarProduct (Point x1 y1) (Point x2 y2) = x1 * x2 + y1 * y2

crossProduct :: Point -> Point -> Int
crossProduct (Point x1 y1) (Point x2 y2) = x1 * y2 - x2 * y1

perimeter :: [Point] -> Double
perimeter [] = 0
perimeter (_:[]) = 0
perimeter a = runEval $ do
  x <- rpar (init a)
  y <- rseq (tail a)
  _ <- rseq x
  a1 <- rpar (len (head a) (last a))
  a2 <- rseq (foldl' (+) 0.0 (zipWith len x y))
  _ <- rseq a1
  return (a1 + a2)

slowPerimeter :: [Point] -> Double
slowPerimeter [] = 0
slowPerimeter (_:[]) = 0
slowPerimeter a = foldl' (+) 0.0 (zipWith len (init a) (tail a)) + (len (head a) (last a))

len :: Point -> Point -> Double
len a b = let (Point x y) = minus a b in sqrt (fromIntegral (x * x + y * y))

simpleDoubleArea :: [Point] -> Int
simpleDoubleArea [] = 0
simpleDoubleArea (a:as) = (doubleAreaHelpPlus a a as) - (doubleAreaHelpMinus a a as)

doubleArea :: [Point] -> Int    -- Считает удвоенную площадь
doubleArea [] = 0
doubleArea (a:as) = runEval $ do
  x <- rpar (runEval $ rdeepseq $ doubleAreaHelpPlus a a as)
  y <- rdeepseq (doubleAreaHelpMinus a a as)
  _ <- rdeepseq x
  return (x - y)

doubleAreaHelpPlus :: Point -> Point -> [Point] -> Int
doubleAreaHelpPlus _ _ [] = 0
doubleAreaHelpPlus (Point _ y1) (Point x2 _) ((Point x3 y3):[]) = x2 * y3 + x3 * y1
doubleAreaHelpPlus a (Point x2 _) ((Point x3 y3):as) = x2 * y3 + doubleAreaHelpPlus a ((Point x3 y3)) as

doubleAreaHelpMinus :: Point -> Point -> [Point] -> Int
doubleAreaHelpMinus _ _ [] = 0
doubleAreaHelpMinus (Point x1 _) (Point _ y2) ((Point x3 y3):[]) = y2 * x3 + y3 * x1
doubleAreaHelpMinus a (Point _ y2) ((Point x3 y3):as) = y2 * x3 + doubleAreaHelpMinus a ((Point x3 y3)) as
