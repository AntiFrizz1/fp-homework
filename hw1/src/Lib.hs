{-# LANGUAGE InstanceSigs #-}

module Lib
       ( randomIntList
       , order3
       , smartReplicate
       , contains
       , stringSum
       ) where

import System.Random (newStdGen, randomRs)

randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = take n . randomRs (from, to) <$> newStdGen


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

--Block 2

erase :: Int -> [a] -> Maybe (a, [a])
erase a (b:bt) = if (a == 0) then Just (b, bt) else erase (a - 1) bt
erase _ [] = Nothing

-- mergeSort :: Ord a => [a] -> [a]
-- mergeSort [] = []

--Block 3

--Task 1

data WeekDays = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Show)

nextDay :: WeekDays -> WeekDays
nextDay Monday = Tuesday
nextDay Tuesday = Wednesday
nextDay Wednesday = Thursday
nextDay Thursday = Friday
nextDay Friday = Saturday
nextDay Saturday = Sunday
nextDay Sunday = Monday

afterDays :: Int -> WeekDays -> WeekDays
afterDays 0 d = d
afterDays a d = afterDays (a - 1) (nextDay d)

isWeekend :: WeekDays -> Bool
isWeekend Saturday = True
isWeekend Sunday = True
isWeekend _ = False

daysToParty :: WeekDays -> Integer
daysToParty Friday = 0
daysToParty d = (daysToParty (nextDay d)) + 1

--Task 2

data Wall = EmptyWall | SimpleWall deriving(Show)

data Lord = EmptyLord | SimpleLord deriving(Show)

data Family = OneMan | TwoMan | ThreeMan | FourMan deriving(Show)

data Houses = OneHouse Family | ManyHouses [Houses] deriving(Show)

data Castle = EmptyCastle | SimpleCastle Wall Lord deriving(Show)

data ChurchOrLibrary = Empty | Church | Library deriving(Show)

data Town = Town
  { castle :: Castle
  , churchOrLibrary :: ChurchOrLibrary
  , houses :: Houses
  } deriving(Show)

buildCastle :: Town -> (Town, Bool)
buildCastle (Town EmptyCastle col houses) = (Town (SimpleCastle EmptyWall EmptyLord) col houses, True)
buildCastle a = (a, False)

buildChurch :: Town -> (Town, Bool)
buildChurch (Town castle Empty houses) = (Town castle Church houses, True)
buildChurch a = (a, False)

buildLibrary :: Town -> (Town, Bool)
buildLibrary (Town castle Empty houses) = (Town castle Library houses, True)
buildLibrary a = (a, False)

insertFamily :: Family -> Town -> Town
insertFamily family (Town castle col (OneHouse family1)) = Town castle col ((ManyHouses ((OneHouse family):(OneHouse family1):[])))
insertFamily family (Town castle col (ManyHouses houses)) = Town castle col ((ManyHouses ((OneHouse family):houses)))

--0 - удачно въехал
--1 - нет замка
--2 - уже есть лорд
insertLord :: Town -> (Town, Int)
insertLord (Town EmptyCastle col houses) = (Town EmptyCastle col houses, 1)
insertLord (Town (SimpleCastle wall SimpleLord) col houses) = (Town (SimpleCastle wall SimpleLord) col houses, 2)
insertLord (Town (SimpleCastle wall EmptyLord) col houses) = (Town (SimpleCastle wall SimpleLord) col houses, 0)

familyPopulation :: Family -> Int
familyPopulation OneMan = 1
familyPopulation TwoMan = 2
familyPopulation ThreeMan = 3
familyPopulation FourMan = 4

housePopulation :: Houses -> Int
housePopulation (OneHouse family) = familyPopulation family
housePopulation (ManyHouses (family:[])) = housePopulation family
housePopulation (ManyHouses (family:families)) = (housePopulation family) + (housePopulation (ManyHouses families))

buildWall :: Town -> Town
buildWall (Town (SimpleCastle EmptyWall SimpleLord) col houses)
  | (housePopulation houses) >= 10 = (Town (SimpleCastle SimpleWall SimpleLord) col houses)
  | otherwise                      = (Town (SimpleCastle EmptyWall SimpleLord) col houses)
buildWall a = a
--Task3

data Nat = Z | S Nat

add :: Nat -> Nat -> Nat
add a Z = a
add a (S b) = S (add a b)

mul :: Nat -> Nat -> Nat
mul a Z = Z
mul a (S b) = add a (mul a b)

sub :: Nat -> Nat -> Nat
sub Z _ = Z
sub x Z = x
sub (S x) (S y) = sub x y

natToInt :: Nat -> Int
natToInt Z = 0
natToInt (S a) = 1 + natToInt a

intToNat :: Int -> Nat
intToNat 0 = Z
intToNat a = S (intToNat (a - 1))

instance Eq Nat where
  (==) Z Z = True
  (==) _ Z = False
  (==) Z _ = False
  (==) (S a) (S b) = a == b

instance Ord Nat where
  compare Z Z = EQ
  compare _ Z = GT
  compare Z _ = LT
  compare (S x) (S y) = compare x y

  x <= y              =  compare x y /= GT
  x <  y              =  compare x y == LT
  x >= y              =  compare x y /= LT
  x >  y              =  compare x y == GT

isEven :: Nat -> Bool
isEven Z = True
isEven (S a) = not (isEven a)

divNat :: Nat -> Nat -> Nat
divNat Z _ = Z
divNat a b
  | a < b  = Z
  | otherwise = S (divNat (sub a b) b)

modNat :: Nat -> Nat -> Nat
modNat a b = sub a (mul (divNat a b) b)

--Task 4

data Tree a = Leaf | Node { dataList :: [a]
                          , left :: Tree a
                          , right :: Tree a
                          } deriving(Show)


isEmpty :: Tree a -> Bool
isEmpty Leaf = True
isEmpty _ = False

size :: Tree a -> Int
size Leaf = 0
size (Node _ left right) = 1 + (size left) + (size right)

findElem :: Ord a => a -> Tree a -> Bool
findElem _ (Leaf) = False
findElem e (Node (a:gs) left right)
  | e == a      = True
  | e < a       = findElem e left
  | otherwise   = findElem e right

insertElem :: Ord a => a -> Tree a -> Tree a
insertElem e Leaf = Node (e:[]) Leaf Leaf
insertElem e (Node (a:gs) left right)
  | e == a      = Node (e:a:gs) left right
  | e < a       = Node (a:gs) (insertElem e left) right
  | otherwise   = Node (a:gs) left (insertElem e right)


fromList :: Ord a => [a] -> Tree a
fromList [] = Leaf
fromList (a:as) = (insertElem a (fromList as))

--Block 4

--Task 1

data NonEmpty a = a :| [a]
data Pair a = Pair a a

instance Foldable Pair where
  foldr :: (a -> b -> b) -> b -> Pair a -> b
  foldr f a (Pair b c) = f b (f c a)

  foldMap :: Monoid m => (a -> m) -> Pair a -> m
  foldMap f (Pair a b) = mappend (f a) (f b)

instance Foldable NonEmpty where
  foldr :: (a -> b -> b) -> b -> NonEmpty a -> b
  foldr f a (b:|[]) = f b a
  foldr f a (b:|c) = foldr f (f b a) c

  foldMap :: Monoid m => (a -> m) -> NonEmpty a -> m
  foldMap f (a:|[]) = mappend (f a) mempty
  foldMap f (a:|as) = mappend (f a) (foldMap f as)

--Block 5

--Task1

maybeConcat :: [Maybe ([a])] -> [a]
maybeConcat [] = []
maybeConcat ((Just a):gs) = a <> (maybeConcat gs)
maybeConcat (Nothing:gs) = maybeConcat gs

--Task 2

instance Semigroup (NonEmpty a) where
  (<>) (a:|[]) (b:|bs) = a:|(b:bs)
  (<>) (a:|as) (b:|[]) = a:|(as <> (b:[]))
  (<>) (a:|as) (b:|bs) = a:|(as <> (b:bs))

data ThisOrThat a b = This a | That b | Both a b

instance (Semigroup a, Semigroup b) => Semigroup (ThisOrThat a b) where
  (<>) (This a) (This b) = This (a <> b)
  (<>) (This a) (That b) = Both a b
  (<>) (That a) (That b) = That (a <> b)
  (<>) (This a) (Both b c) = Both (a <> b) c
  (<>) (That a) (Both b c) = Both b (a <> c)
  (<>) (Both a b) (This c) = Both (a <> c) b
  (<>) (Both a b) (That c) = Both a (b <> c)
  (<>) (Both a b) (Both c d) = Both (a <> c) (b <> d)

data Name = Name String deriaving(Show)

instance Semigroup Name where
  (<>) (Name a) (Name b) = Name (a <> "." <> b)

--Task 3

data Builder = One Char | Many [Builder] deriving(Show)

instance Semigroup Builder where
  (<>) (One a) (One b) = Many ((One a):(One b):[])
  (<>) (One a) (Many b) = Many ((One a):b)
  (<>) (Many a) (One b) = Many (a <> ((One b):[]))
  (<>) (Many a) (Many b) = Many (a <> b)

instance Monoid Builder where
  mempty = One ' '

fromString :: String -> Builder
fromString (a:[]) = One a
fromString (a:as) = (One a) <> (fromString as)

toString :: Builder -> String
toString (One a) = a:[]
toString (Many (a:[])) = toString a
toString (Many (a:as)) = (toString a) <> (toString (Many as))
