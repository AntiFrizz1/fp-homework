module Task3
       ( composition
       , identity
       , contraction
       , permutation
       ) where

s :: (a -> b -> c) -> (a -> b) -> a -> c
s f g x = f x (g x)
--из курса по ТТ
--комбинатор B
composition :: (b -> c) -> (a -> b) -> a -> c
composition = s (const s) const

--комбинатор I
identity :: a -> a
identity = s const const

--комбинатор W
contraction :: (a -> a -> b) -> a -> b
contraction = s s (const (s const const))
--комбинатор C
permutation :: (a -> b -> c) -> b -> a -> c
permutation = s ((s (const s) const) (s (const s) const) s) (const const)
