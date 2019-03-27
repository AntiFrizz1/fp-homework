module Block3.Task1
  ( Parser (..)
  , runP
  ) where

import Control.Applicative (Alternative (..))

data Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

first :: (a -> c) -> (a, b) -> (c, b)
first f (r, s) = (f r, s)

runP :: Parser s a -> [s] -> Maybe (a, [s])
runP (Parser pf) s = pf s

instance Functor (Parser s) where
  fmap f (Parser pa) = Parser (fmap (first f) . pa)

instance Applicative (Parser s) where
  pure a                  = Parser (\s -> Just (a, s))

  Parser pf <*> Parser ps = Parser $ \s -> do
    (a, b)   <- pf s
    (a1, b1) <- ps b
    pure (a a1, b1)

instance Monad (Parser s) where
  (Parser p) >>= qf = Parser $ \s -> do
    (r, t) <- p s
    runP (qf r) t

  return            = pure

instance Alternative (Parser s) where
  empty                   = Parser (\_ -> Nothing)

  Parser pa <|> Parser pb = Parser (\s -> pa s <|> pb s)
