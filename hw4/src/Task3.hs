{-# LANGUAGE BangPatterns #-}
module Task3
  ( gauss
  , verifySolution
  ) where

import Data.Bits (xor)
import Data.List (zipWith, foldl', span)
import Control.Parallel.Strategies (parMap, rdeepseq)

gauss :: [[Bool]] -> [Bool] -> Maybe [Bool]
gauss x y = let !a = zip x y in
  let !s = length a in
  getAnswer (gaussHelp' s (gaussHelp 0 s a)) >>= (\q -> Just $ reverse q)

gaussHelp :: Int -> Int -> [([Bool], Bool)] -> [([Bool], Bool)]
gaussHelp _ _ [] = []
gaussHelp n s a = if n == s then a else case getter n a of
    Nothing -> gaussHelp (n + 1) s a
    Just (as', a', b') -> let !h = (parMap rdeepseq (processVector n (a', b')) as') in (a', b'):(gaussHelp (n + 1) s h)

gaussHelp' :: Int -> [([Bool], Bool)] -> [([Bool], Bool)]
gaussHelp' 0 a = a
gaussHelp' n a = case getter n a of
    Nothing -> gaussHelp'  (n - 1) a
    Just (as', a', b') -> let !h = (parMap rdeepseq (processVector' n (a', b')) as') in (a', b'):(gaussHelp' (n - 1) h)

getAnswer :: [([Bool], Bool)] -> Maybe [Bool]
getAnswer a =
  let !help = (parMap rdeepseq (\(x, y) -> findOnlyFirstAtVector x >>= (\p -> Just (y, p))) a) in
  let !help1 = makeVector (reverse  help) 0 (length help) in
    case sequenceA help1 of
      Nothing -> let !h = fmap (\s -> case s of
                                        Nothing -> (False, False)
                                        Just q -> (q, True)
                                      ) help1 in findAnswer a h
      Just b -> Just b

findOnlyFirstAtVector :: [Bool] -> Maybe Int
findOnlyFirstAtVector a = case (span ((==) False) a) of
    (_,[]) -> Nothing
    (b,(_:q)) -> (case (span ((==) False) q) of
      (_,[]) -> Just (length b)
      (_,_) -> Nothing)

makeVector :: [Maybe (Bool, Int)] -> Int -> Int -> [Maybe Bool]
makeVector [] n s =  addNothing (s - n) []
makeVector (Nothing:as) n s = makeVector as n s
makeVector ((Just (p, i)):as) n s = addNothing (i - n) ((Just p):(makeVector as (i + 1) s))

addNothing :: Int -> [Maybe Bool] -> [Maybe Bool]
addNothing 0 a = a
addNothing n a = if (n < 0) then [] else Nothing:(addNothing (n - 1) a)


processVector :: Int -> ([Bool], Bool) -> ([Bool], Bool) -> ([Bool], Bool)
processVector n (v1, av1) (v2, av2) = case findFirstAtVector v2 of
  Nothing -> (v2, av2)
  Just p -> if (p == n) then (xorVectors v1 v2, av1 `xor` av2)
    else (v2, av2)

getter :: Int -> [([Bool], Bool)] -> Maybe ([([Bool], Bool)], [Bool], Bool)
getter _ [] = Nothing
getter n ((a, b):as) = case findFirstAtVector a of
  Nothing -> getter n as >>= (\(as', a', b') -> Just ((a,b):as', a', b'))
  Just n' -> if (n' == n) then return $ (as, a, b)
    else getter n as >>= (\(as', a', b') -> Just ((a,b):as', a', b'))

processVector' :: Int -> ([Bool], Bool) -> ([Bool], Bool) -> ([Bool], Bool)
processVector' n (v1, av1) (v2, av2) = if v2 !! n then (xorVectors v1 v2, av1 `xor` av2)
  else (v2, av2)

findFirstAtVector :: [Bool] -> Maybe Int
findFirstAtVector a = case (span ((==) False) a) of
    (_,[]) -> Nothing
    (b,_) -> Just (length b)

xorVectors :: [Bool] -> [Bool] -> [Bool]
xorVectors a b = zipWith xor a b

findAnswer :: [([Bool],Bool)] -> [(Bool, Bool)] -> Maybe [Bool]
findAnswer a b = case verifyAnswer a b of
  False -> genNext b >>= (\s -> findAnswer a s)
  True -> Just (fmap (\(x1, _) -> x1) b)

genNext :: [(Bool, Bool)] -> Maybe [(Bool, Bool)]
genNext [] = Nothing
genNext ((a, True):as) = (genNext as) >>= (\s -> Just ((a, True):s))
genNext ((True, False):as) = genNext as >>= (\s -> Just ((False, False):s))
genNext ((False, False):as) = Just ((True, False):as)

verifyAnswer :: [([Bool], Bool)] -> [(Bool, Bool)] -> Bool
verifyAnswer a b = foldl' (&&) True (parMap rdeepseq (\(x', ans) -> verifyLine' x' ans b) a)

verifyLine' :: [Bool] -> Bool -> [(Bool, Bool)] -> Bool
verifyLine' [] _ _ = True
verifyLine' _ _ [] = False
verifyLine' l a x = (let !h = (zipWith (\s -> \(x', _) -> s && x') l x) in foldl' xor (head h) (tail h)) == a



verifySolution :: [[Bool]] -> [Bool] -> [Bool] -> Bool
verifySolution (a) (b) ans = let !x = verifySolutionHelp a b in
  let !p = parMap rdeepseq (\s -> s ans) x in foldl' (&&) True p

verifySolutionHelp :: [[Bool]] -> [Bool] -> [[Bool] -> Bool]
verifySolutionHelp (a) (b) = zipWith (\x -> \y -> (verifyLine x y)) a b

verifyLine :: [Bool] -> Bool -> [Bool] -> Bool
verifyLine [] _ _ = True
verifyLine _ _ [] = False
verifyLine l a x = (let h = (zipWith (&&) l x) in foldl' xor (head h) (tail h)) == a
