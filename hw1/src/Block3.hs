module Block3
  ( nextDay
  , afterDays
  , isWeekend
  , daysToParty
  , buildWall
  , buildCastle
  , buildChurch
  , buildLibrary
  , insertFamily
  , insertLord
  , add
  , mul
  , sub
  , natToInt
  , intToNat
  , Nat (..)
  , isEven
  , divNat
  , modNat
  , WeekDays (..)
  , NonEmpty (..)
  , Wall (..)
  , Lord (..)
  , Family (..)
  , Houses (..)
  , Castle (..)
  , ChurchOrLibrary (..)
  , Town (..)
  , Tree (..)
  , fromList
  , insertElem
  , findElem
  , size
  , isEmpty
  ) where

--Block 3
--Task 1

data WeekDays = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Show)

nextDay :: WeekDays -> WeekDays
nextDay Monday    = Tuesday
nextDay Tuesday   = Wednesday
nextDay Wednesday = Thursday
nextDay Thursday  = Friday
nextDay Friday    = Saturday
nextDay Saturday  = Sunday
nextDay Sunday    = Monday

afterDays :: Int -> WeekDays -> WeekDays
afterDays 0 d = d
afterDays a d = afterDays (a - 1) (nextDay d)

isWeekend :: WeekDays -> Bool
isWeekend Saturday = True
isWeekend Sunday   = True
isWeekend _        = False

daysToParty :: WeekDays -> Integer
daysToParty Friday = 0
daysToParty d      = (daysToParty (nextDay d)) + 1

--Task 2

data NonEmpty a = a :| [a] deriving(Show)

data Wall = EWall | SWall

data Lord = ELord | SLord

data Family = OneMan | TwoMan | ThreeMan | FourMan

data Houses = Houses (NonEmpty Family)

data Castle = ECastle | SCastle Wall Lord

data ChurchOrLibrary = Empty | Church | Library

data Town = Town
  { castleElement   :: Castle
  , churchOrLibrary :: ChurchOrLibrary
  , housesElement   :: Houses
  }

buildCastle :: Town -> (Town, Bool)
buildCastle (Town ECastle cl h) = (Town (SCastle EWall ELord) cl h, True)
buildCastle a                   = (a, False)

buildChurch :: Town -> (Town, Bool)
buildChurch (Town ct Empty h) = (Town ct Church h, True)
buildChurch a                 = (a, False)

buildLibrary :: Town -> (Town, Bool)
buildLibrary (Town ct Empty h) = (Town ct Library h, True)
buildLibrary a                 = (a, False)

insertFamily :: Family -> Town -> Town
insertFamily f (Town ct cl (Houses (f1:|fs))) = Town ct cl (Houses (f:|(f1:fs)))

--0 - удачно въехал
--1 - нет замка
--2 - уже есть лорд
insertLord :: Town -> (Town, Int)
insertLord (Town ECastle cl h)            = (Town ECastle cl h, 1)
insertLord (Town (SCastle w SLord) cl h)  = (Town (SCastle w SLord) cl h, 2)
insertLord (Town (SCastle w ELord) cl h)  = (Town (SCastle w SLord) cl h, 0)

fCount :: Family -> Int
fCount OneMan   = 1
fCount TwoMan   = 2
fCount ThreeMan = 3
fCount FourMan  = 4

hCount :: Houses -> Int
hCount (Houses (a:|as)) = foldr (+) (fCount a) (map (fCount) as)

buildWall :: Town -> Town
buildWall (Town (SCastle EWall SLord) cl h)
  | (hCount h) >= 10 = (Town (SCastle SWall SLord) cl h)
  | otherwise        = (Town (SCastle EWall SLord) cl h)
buildWall a = a

--Task3

data Nat = Z | S Nat

add :: Nat -> Nat -> Nat
add a Z     = a
add a (S b) = S (add a b)

mul :: Nat -> Nat -> Nat
mul _ Z     = Z
mul a (S b) = add a (mul a b)

sub :: Nat -> Nat -> Nat
sub Z _         = Z
sub x Z         = x
sub (S x) (S y) = sub x y

natToInt :: Nat -> Int
natToInt Z     = 0
natToInt (S a) = 1 + natToInt a

intToNat :: Int -> Nat
intToNat 0 = Z
intToNat a = S (intToNat (a - 1))

instance Eq Nat where
  (==) Z Z         = True
  (==) _ Z         = False
  (==) Z _         = False
  (==) (S a) (S b) = a == b

instance Ord Nat where
  compare Z Z         = EQ
  compare _ Z         = GT
  compare Z _         = LT
  compare (S x) (S y) = compare x y

  x <= y              =  compare x y /= GT
  x <  y              =  compare x y == LT
  x >= y              =  compare x y /= LT
  x >  y              =  compare x y == GT

isEven :: Nat -> Bool
isEven Z     = True
isEven (S a) = not (isEven a)

divNat :: Nat -> Nat -> Nat
divNat Z _    = Z
divNat a b
  | a < b     = Z
  | otherwise = S (divNat (sub a b) b)

modNat :: Nat -> Nat -> Nat
modNat a b = sub a (mul (divNat a b) b)



--Task 4

data Tree a = Leaf | Node
  { dataList :: NonEmpty a
  , l        :: Tree a
  , r        :: Tree a
  } deriving(Show)


isEmpty :: Tree a -> Bool
isEmpty Leaf = True
isEmpty _    = False

size :: Tree a -> Int
size Leaf                = 0
size (Node _ left right) = 1 + (size left) + (size right)

findElem :: Ord a => a -> Tree a -> Bool
findElem _ (Leaf) = False
findElem e (Node (a:|_) left right)
  | e == a      = True
  | e < a       = findElem e left
  | otherwise   = findElem e right

insertElem :: Ord a => a -> Tree a -> Tree a
insertElem e Leaf = Node (e:|[]) Leaf Leaf
insertElem e (Node (a:|gs) left right)
  | e == a      = Node (e:|(a:gs)) left right
  | e < a       = Node (a:|gs) (insertElem e left) right
  | otherwise   = Node (a:|gs) left (insertElem e right)


fromList :: Ord a => [a] -> Tree a
fromList []     = Leaf
fromList (a:as) = (insertElem a (fromList as))
