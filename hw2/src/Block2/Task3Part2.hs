{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Block2.Task3Part2
  ( MonadJoin(..)
  , Monad(..)
  ) where

import Prelude hiding(Monad, (>>=), return)

class MonadJoin m where
  returnJoin :: a -> m a
  join       :: m (m a) -> m a

class Monad m where
  (>>=)  :: m a -> (a -> m b) -> m b
  return :: a -> m a

instance Monad m => MonadJoin m where
  returnJoin = return

  join a     = a >>= id
