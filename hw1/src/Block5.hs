module Block5
  ( maybeConcat
  , fromString
  , toString
  , ThisOrThat (..)
  , Builder (..)
  ) where

data NonEmpty a = a :| [a]
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
  (<>) (This a) (This b)     = This (a <> b)
  (<>) (This a) (That b)     = Both a b
  (<>) (This a) (Both b c)   = Both (a <> b) c
  (<>) (That a) (That b)     = That (a <> b)
  (<>) (That a) (Both b c)   = Both b (a <> c)
  (<>) (That a) (This b)     = Both b a
  (<>) (Both a b) (This c)   = Both (a <> c) b
  (<>) (Both a b) (That c)   = Both a (b <> c)
  (<>) (Both a b) (Both c d) = Both (a <> c) (b <> d)

data Name = Name String deriving(Show)

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
