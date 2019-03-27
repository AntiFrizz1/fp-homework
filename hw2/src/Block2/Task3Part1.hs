{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Block2.Task3Part1
  ( MonadFish(..)
  , MonadJoin(..)
  , Monad(..)
  ) where

import Prelude hiding(Monad, (>>=), return)

class MonadFish m where
  returnFish :: a -> m a
  (>=>)      :: (a -> m b) -> (b -> m c) -> (a -> m c)

class MonadJoin m where
  returnJoin :: a -> m a
  join       :: m (m a) -> m a

class Monad m where
  (>>=)  :: m a -> (a -> m b) -> m b
  return :: a -> m a

instance MonadFish m => MonadJoin m where
  returnJoin = returnFish

  join a     = (id >=> id) a

instance MonadFish m => Monad m where
  return    = returnFish

  (>>=) a f = (id >=> f) a

instance Monad m => MonadFish m where
  returnFish = return

  (>=>) f g  = (\x -> (return x) >>= f >>= g)
