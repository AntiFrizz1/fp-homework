module Block2
  ( randomIntList
  , erase
  ) where

--Block 2
import System.Random (newStdGen, randomRs)

randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = take n . randomRs (from, to) <$> newStdGen


erase :: Int -> [a] -> Maybe (a, [a])
erase a (b:bt) =
  if (a == 0)
  then
    Just (b, bt)
  else
    erase (a - 1) bt
erase _ []     = Nothing

-- mergeSort :: Ord a => [a] -> [a]
-- mergeSort [] = []
