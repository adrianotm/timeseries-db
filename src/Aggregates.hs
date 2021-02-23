module Aggregates where

import           Data.Foldable
import           Repository.Model

newtype Collect n = Collect { getList :: [n] }

toCollect :: a -> Collect a
toCollect a = Collect [a]

instance Semigroup (Collect n) where
  Collect x <> Collect y = Collect $ x ++ y

instance Monoid (Collect n) where
  mappend=(<>)
  mempty = Collect []

------
data Average n = Average { length :: !Int, sum :: !n }

toAvg :: a -> Average a
toAvg = Average 1

getAverage :: (Num n, Fractional n) => Average n -> Maybe n
getAverage (Average l n) | l == 0 = Nothing
                         | otherwise = Just $ n / fromIntegral l

instance Num n => Semigroup (Average n) where
  Average lx nx <> Average ly ny = Average (lx + ly) (nx + ny)

instance Num n => Monoid (Average n) where
  mappend = (<>)
  mempty = Average 0 0
