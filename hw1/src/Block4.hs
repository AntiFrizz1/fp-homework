{-# LANGUAGE InstanceSigs #-}

module Block4
  ( NonEmpty (..)
  , Pair (..)
  ) where


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
