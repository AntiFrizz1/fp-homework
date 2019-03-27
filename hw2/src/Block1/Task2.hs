module Block1.Task2
  ( Tree(..)
  , NonEmpty(..)
  ) where

import Data.Maybe (fromJust)

data Tree a
  = Branch (Tree a) (Tree a)
  | Leaf a
  deriving (Show)

instance Functor Tree where
  fmap f (Leaf a)            = Leaf (f a)
  fmap f (Branch left right) = Branch (fmap f left) (fmap f right)

  (<$) a (Leaf _)            = Leaf a
  (<$) a (Branch left right) = Branch (a <$ left) (a <$ right)

instance Applicative Tree where
  pure a               = Leaf a

  (<*>) (Leaf f) a     = fmap f a
  (<*>) (Branch a b) c = Branch (a <*> c) (b <*> c)

instance Monad Tree where
  (>>=) (Leaf a) f            = f a
  (>>=) (Branch left right) f = Branch (left >>= f) (right >>= f)

  return a                    = pure a

instance Foldable Tree where
  foldr f a (Leaf b) = f b a
  foldr f a (Branch left right) = foldr f (foldr f a left) right

instance Traversable Tree where
  traverse f (Leaf a)     = Leaf <$> f a
  traverse f (Branch a b) = pure Branch <*> traverse f a <*> traverse f b

data NonEmpty a = a :| [a]
  deriving(Show)

fromList :: [a] -> Maybe (NonEmpty a)
fromList (a:[]) = Just (a:|[])
fromList (a:as) = Just (a:|as)
fromList []     = Nothing

toList :: NonEmpty a -> [a]
toList (a:|as) = a:as

concatList :: NonEmpty a -> NonEmpty a -> NonEmpty a
concatList (a:|[]) (b:|bs) = a:|(b:bs)
concatList (a:|(ax:as)) b  = a:| toList (concatList (ax:|as) b)

join :: NonEmpty (NonEmpty a) -> NonEmpty a
join (a:|[]) = a
join (a:|(ax:as)) = concatList a (join (ax:|as))

instance Functor NonEmpty where
  fmap f (a:|b)  = (f a) :| (fmap f b)

  (<$) a (_:|bs) = a :| ((<$) a bs)

instance Applicative NonEmpty where
  pure a                = a:|[]

  (<*>) (f:|fs) (a:|as) = let list = f:fs <*> a:as in (head list):|(tail list)

instance Monad NonEmpty where
  return a = pure a

  (>>=) (a:|[]) f = f a
  (>>=) b f       = join (fmap f b)

instance Foldable NonEmpty where
  foldr f a (b:|[]) = f b a
  foldr f a (b:|as) = f b (foldr f a as)

instance Traversable NonEmpty where
  traverse f (a:|as) = fromJust <$> fromList <$>  traverse f (a:as)
