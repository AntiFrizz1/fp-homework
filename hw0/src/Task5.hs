module Task5
       ( zero
       , succChurch
       , churchMult
       , churchPlus
       , churchToInt
       ) where
--Task5

type Nat a = (a -> a) -> a -> a

zero :: Nat a
zero _ x = x

--из курса по тт
succChurch :: Nat a -> Nat a
succChurch n f x = f (n f x)

--из курса по тт
churchPlus, churchMult
    :: Nat a -> Nat a -> Nat a
churchPlus a b f x = a f (b f x)
churchMult a b f x = a (b f) x

churchToInt :: Nat Integer -> Integer
churchToInt n = n ((+) 1) 0
