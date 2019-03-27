module Block3.Task2
  ( ok
  , eof
  , satisfy
  , element
  , stream
  ) where

import Block3.Task1 (Parser(..))
import Control.Applicative (Alternative (..))

ok :: Parser s ()
ok = Parser $ \s -> Just ((), s)

eof :: Parser s ()
eof = Parser $ \s ->
  case s of
    [] -> Just ((), [])
    _  -> Nothing

satisfy :: (s -> Bool) -> Parser s s
satisfy p = Parser f where
  f [] = Nothing
  f (x:xs)
    | p x        = Just (x, xs)
    | otherwise  = Nothing

element ::Eq s => s -> Parser s s
element s = satisfy (== s)

stream :: Eq s => [s] -> Parser s [s]
stream []     = empty
stream (a:[]) = Parser f where
  f [] = Nothing
  f (q:qs)
    | a == q    = Just (a:[], qs)
    | otherwise = Nothing

stream (a:as) = Parser f where
  f [] = Nothing
  f (q:qs)
    | a == q    = do
      let (Parser fs) = stream as
      (dx, ds) <- fs qs
      Just (a:dx, ds)
    | otherwise = Nothing
