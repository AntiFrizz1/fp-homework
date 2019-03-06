module Task4
       ( iterateElement
       , fibonacci
       , factorial
       , mapFix
       ) where

import Data.Function (fix)
--import Data.Maybe (mapMaybe)
--Task4

iterateElement :: a -> [a]
iterateElement = fix (\f -> \n -> n : (f n))

fibonacci :: Integer -> Integer
fibonacci = fix (\f -> (\n -> if ((n == 0) || (n == 1)) then 1 else ((f (n - 1)) + (f (n - 2)))))

--с курса по тт
factorial :: Integer -> Integer
factorial = fix (\f -> (\n -> if (n == 0) then 1 else n * (f (n - 1))))

isEmptyList :: [a] -> Bool
isEmptyList [] = True
isEmptyList _  = False

mapFixPrime :: (a -> b) -> [a] -> [b]
mapFixPrime = fix (\f -> \g -> \(a:gs) -> if isEmptyList gs then (g a):[] else (g a) : (f g gs))

mapFix :: (a -> b) -> [a] -> [b]
mapFix _ [] = []
mapFix f a  = mapFixPrime f a
