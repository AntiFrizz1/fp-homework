{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Task5
    ( someFunc
    , view
    , Lens
    , Lens'
    , personAddressLens
    , Person (..)
    , Address (..)
    , Street (..)
    , City (..)
    , set
    , over
    , _1
    , _2
    , lens
    , choosing
    , (<<%~)
    , (<%~)
    , Mem(..)
    ) where

import Data.Functor.Identity (Identity(..), runIdentity)
import Control.Applicative (getConst, Const(..))

data Person = Person
     { name'    :: String
     , age     :: Int
     , address :: Address
     }  deriving (Show)

data Address = Address
     { house  :: Int
     , street :: Street
     , city   :: City
     } deriving (Show)

newtype City = City String deriving (Show)
newtype Street = Street String deriving (Show)

type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> f t

type Lens' s a = Lens s s a a

personAddressLens :: Lens' Person Address
personAddressLens = lens address (\ob -> \ad -> ob {address = ad})

set :: Lens' s a -> a -> s -> s         -- set    value (setter)
set a b c = runIdentity $ a (\_ -> Identity b) c

view :: Lens' s a -> s -> a              -- lookup value (getter)
view a v = getConst $ a Const v

over :: Lens' s a -> (a -> a) -> s -> s  -- change value (modifier)
over a b c = runIdentity $ a (Identity . b) c

-- _1 :: Functor f => (a -> f b) -> (a, x) -> f (b, x)
_1 :: Lens (a, x) (b, x) a b
_1 func (fs, sd) = fmap (\b -> (b, sd)) (func fs)

-- _2 :: Functor f => (a -> f b) -> (x, a) -> f (x, b)
_2 :: Lens (x, a) (x, b) a b
_2 func (fs, sd) = fmap (\b -> (fs, b)) (func sd)

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens geter seter func obj = fmap (\ad -> seter obj ad) (func $ geter obj)

-- Объединить две линзы в одну, которая работает с Either.
choosing :: Lens s1 t1 a b
         -> Lens s2 t2 a b
         -> Lens (Either s1 s2) (Either t1 t2) a b
choosing l1 l2 func obj = case obj of
  Left a -> fmap Left (l1 func a)
  Right b -> fmap Right (l2 func b)

-- Изменить цель линзы и вернуть новый результат. Постарайтесь
-- реализовать без анонимных функций и определений своих функций
(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~) l f s = (f (getConst $ l Const s), runIdentity $ l (Identity . f) s)

-- Изменить цель линзы, но вернуть старый результат.
(<<%~) :: Lens s t a b -> (a -> b) -> s -> (a, t)
(<<%~) l f s = (getConst $ l Const s, runIdentity $ l (Identity . f) s)


data Mem = Mem
  { memName :: String
  , date :: String
  , like :: Bool
  } deriving (Show)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
