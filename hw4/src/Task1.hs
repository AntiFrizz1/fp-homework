{-# LANGUAGE BangPatterns #-}
module Task1
  ( multiply
  , simpleMultiply
  ) where


import Control.Parallel.Strategies (parMap, rdeepseq)
import Data.List (foldl', transpose)

simpleMultiply :: [[Int]] -> [[Int]] -> Maybe [[Int]]
simpleMultiply a b = case checkInput a b of
  False -> Nothing
  True -> let !q = transpose b in Just (fmap (\s -> let !p = mulVectorToMatrix s q in p) a)

multiply :: [[Int]] -> [[Int]] -> Maybe [[Int]]
multiply a b = case checkInput a b of
  False -> Nothing
  True -> let !q = transpose b in Just (parMap rdeepseq (\s -> let !p = mulVectorToMatrix s q in p) a)

checkInput :: [[Int]] -> [[Int]] -> Bool
checkInput _ [] = False
checkInput [] _ = False
checkInput (a:_) (b:bs) = (length a == 1 + length bs) && (length b /= 0)

mulVectorToMatrix :: [Int] -> [[Int]] -> [Int]
mulVectorToMatrix a b = (fmap (\s -> mulVec1 a s) b)

mulVec1 :: [Int] -> [Int] -> Int
mulVec1 a b = foldl' (+) 0 (zipWith (\p -> \q -> p * q) a b)
