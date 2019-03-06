{-# LANGUAGE TypeOperators #-}

module Task1
  ( distributivity
  , associator
  , eitherAssoc
  ) where

--Task1

distributivity :: Either a (b, c) -> (Either a b, Either a c)
distributivity (Left a)       = (Left a, Left a)
distributivity (Right (b, c)) = (Right b, Right c)

associator :: (a, (b, c)) -> ((a, b), c)
associator (a, (b, c)) = ((a, b), c)

type (<->) a b = (a -> b, b -> a)

eitherAssocImplFirst :: Either a (Either b c) -> Either (Either a b) c
eitherAssocImplFirst (Left a)          = Left (Left a)
eitherAssocImplFirst (Right (Left b))  = Left (Right b)
eitherAssocImplFirst (Right (Right c)) = Right c

eitherAssocImplSecond :: Either (Either a b) c -> Either a (Either b c)
eitherAssocImplSecond (Left (Left a))  = Left a
eitherAssocImplSecond (Left (Right b)) = Right (Left b)
eitherAssocImplSecond (Right c)        = Right (Right c)

eitherAssoc :: Either a (Either b c) <-> Either (Either a b) c
eitherAssoc = (eitherAssocImplFirst, eitherAssocImplSecond)
