module Block1.Task1
  ( stringSum
  ) where

import Text.Read (readMaybe)

fromStringToListMaybe :: String -> [Maybe Int]
fromStringToListMaybe s = map (\x -> (readMaybe x) :: Maybe Int) (words s)

adder :: Maybe [Int] -> Maybe Int
adder Nothing  = Nothing
adder (Just x) = Just (foldr (+) 0 x)

stringSum :: String -> Maybe Int
stringSum s = adder (sequenceA (fromStringToListMaybe s))
