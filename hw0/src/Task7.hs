module Task7
       (
       ) where

--Task 7

--null . head $ map (uncurry id) [((++) "Dorian ", " Grey")]
--"Dorian "                                                      :: [Char]
--" Grey"                                                        :: [Char]
--(++)                                                           :: [Char] -> [Char] -> [Char]
--(++) "Dorian "                                                 :: [Char] -> [Char]
--((++) "Dorian ", " Grey")                                      :: ([Char] -> [Char], [Char])
--[((++) "Dorian ", " Grey")]                                    :: [([Char] -> [Char], [Char])]
--id                                                             :: a -> a
--uncurry                                                        :: (a -> [Char] -> [Char]) -> (a, [Char]) -> [Char]
--(uncurry id)                                                   :: ([Char] -> [Char], [Char]) -> [Char]
--map                                                            :: (([Char] -> [Char], [Char]) -> [Char]) -> [([Char] -> [Char], [Char])] -> [[Char]]
--map (uncurry id) [((++) "Dorian ", " Grey")]                   :: [[Char]]
--head                                                           :: [[Char]] -> [Char]
--head $ map (uncurry id) [((++) "Dorian ", " Grey")]            :: [Char]
--null                                                           :: [Char] -> Bool
--null . head $ map (uncurry id) [((++) "Dorian ", " Grey")]     :: Bool

--(\x -> zip (lefts x) (rights x)) [Left (1 + 2), Right (2 ^ 6)]
--1 + 2                                                          :: Num a => a
--2 ^ 6                                                          :: Num a => a
--Left (1 + 2)                                                   :: Num a => Either a b
--Right (2 ^ 6)                                                  :: Num b => Either a b
--[Left (1 + 2), Right (2 ^ 6)]                                  :: (Num a, Num b) => [Either a b]
--x                                                              :: (Num a, Num b) => [Either a b]
--rights x                                                       :: (Num a, Num b) => [Either a b] -> [b]
--lefts x                                                        :: (Num a, Num b) => [Either a b] -> [a]
--zip                                                            :: (Num a, Num b) => [a] -> [b] -> [(a, b)]
--zip (lefts x) (rights x)                                       :: (Num a, Num b) => [(a, b)]
--(\x -> zip (lefts x) (rights x))                               :: (Num a, Num b) => [Either a b] -> [(a, b)]
--(\x -> zip (lefts x) (rights x)) [Left (1 + 2), Right (2 ^ 6)] :: (Num a, Num b) => [(a, b)]

-- let impl = \x y -> not x || y in
--     let isMod2 = \x -> x `mod` 2 == 0 in
--     let isMod4 = \x -> x `mod` 4 == 0 in
--     \x -> (isMod4 x) `impl` (isMod2 x)

--not                                    :: Bool
--(||)                                   :: Bool -> Bool -> Bool
--x                                      :: Bool
--y                                      :: Bool
--not x                                  :: Bool
--not x || y                             :: Bool
--(\x y -> not x || y)                   :: Bool -> Bool -> Bool
--impl                                   :: Bool -> Bool -> Bool
--mod                                    :: Integral a => a -> a -> a
--x                                      :: Integral a => a
--2                                      :: Integral a => a
--0                                      :: Integral a => a
--(==)                                   :: Eq a => a -> a -> Bool
--x 'mod' 2                              :: Integral a => a
--x `mod` 2 == 0                         :: Bool
--(\x -> x `mod` 2 == 0)                 :: Integral a => a -> Bool
--4                                      :: Integral a => a
--x 'mod' 4                              :: Integral a => a
--x `mod` 4 == 0                         :: Bool
--(\x -> x `mod` 4 == 0)                 :: Integral a => a -> Bool
--(isMod2 x)                             :: Bool
--(isMod4 x)                             :: Bool
--(isMod4 x) `impl` (isMod2 x)           :: Bool
--(\x -> (isMod4 x) `impl` (isMod2 x))   :: Integral a => a -> Bool
