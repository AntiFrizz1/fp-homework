module Block1
  ( order3
  , smartReplicate
  , contains
  , stringSum
  ) where

--Block 1

order3 :: Ord a => (a, a, a) -> (a, a, a)
order3 (a, b, c)
  | (a <= b) && (b <= c) = (a, b, c)
  | (a <= c) && (c <= b) = (a, c, b)
  | (b <= a) && (a <= c) = (b, a, c)
  | (b <= c) && (c <= a) = (b, c, a)
  | (c <= a) && (a <= b) = (c, a, b)
  | otherwise            = (c, b, a)

smartReplicate :: [Int] -> [Int]
smartReplicate a = concat (foldr (:) [] (map (\n -> replicate n n) a))

contains :: Eq a => a -> [[a]] -> [[a]]
contains e l = filter (\a -> elem e a) l

stringSum :: String -> Int
stringSum str = foldr (+) 0 (map (\x -> read x::Int) (words str))
