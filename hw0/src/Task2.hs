module Task2
       ( doubleNeg
       , excludedNeg
       , pierce
       , doubleNegElim
       , thirdNegElim
       ) where

import Data.Void (Void)

type Neg a = a -> Void

doubleNeg :: a -> Neg (Neg a)
doubleNeg a b = b a

--из курса матлога
--(!(A|!A))->!A
--((Either a (a -> Void)) -> Void) -> a -> Void
firstLemma :: Neg (Either a (Neg a)) -> Neg a
firstLemma f a = f (Left a)

--(!(A|!A))->!(!A)
--((Either a (a -> Void)) -> Void) -> (a -> Void) -> Void
secondLemma :: Neg (Either a (Neg a)) -> Neg (Neg a)
secondLemma f a = f (Right a)

--((!(A|!A))->!A)->((!(A|!A))->!(!A))->(!!(A|!A)) (9-я схема аксиом)
--(((Either a (a -> Void)) -> Void) -> a -> Void) -> ((((Either a (a -> Void)) -> Void) -> (a -> Void) -> Void)) -> ((Either a (a -> Void)) -> Void) -> Void
thirdLemma :: (Neg (Either a (Neg a)) -> Neg a) -> (Neg (Either a (Neg a)) -> Neg (Neg a)) -> (Neg (Neg (Either a (Neg a))))
thirdLemma f g b = g b (f b)

--((!(A|!A))->!(!A))->(!!(A|!A)) MP thirdLemma, firstLemma
--((((Either a (a -> Void)) -> Void) -> (a -> Void) -> Void)) -> ((Either a (a -> Void)) -> Void) -> Void
fourLemma :: (Neg (Either a (Neg a)) -> Neg (Neg a)) -> (Neg (Neg (Either a (Neg a))))
fourLemma = thirdLemma firstLemma

--(!!(A|!A)) MP fourLemma, secondLemma
--((Either a (a -> Void)) -> Void) -> Void
excludedNeg :: Neg (Neg (Either a (Neg a)))
excludedNeg = (fourLemma secondLemma)

--не заселяемо
pierce :: ((a -> b) -> a) -> a
pierce = undefined

--не заселяемо
doubleNegElim :: Neg (Neg a) -> a
doubleNegElim = undefined

thirdNegElim :: Neg (Neg (Neg a)) -> Neg a
thirdNegElim a b = a (doubleNeg b)
