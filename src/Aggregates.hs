module Aggregates where

newtype Collect n = Collect { getList :: [n] }

toCollect :: a -> Collect a
toCollect a = Collect [a]

instance Semigroup (Collect n) where
  Collect x <> Collect y = Collect $ x ++ y

instance Monoid (Collect n) where
  mappend=(<>)
  mempty = Collect []

