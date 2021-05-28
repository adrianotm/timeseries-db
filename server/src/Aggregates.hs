{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Aggregates where

import           Control.DeepSeq  (NFData)
import           GHC.Generics
import           Repository.Model (AggR (AggR), QueryR (..), TS, Val)

-- | A Monoid for getting the average value
data Average n = Average {length :: !Int, sum :: !n}
  deriving (Generic, NFData)

toAvg :: a -> Average a
toAvg = Average 1
{-# INLINE toAvg #-}

getAverage :: (Num n, Fractional n) => Average n -> Maybe n
getAverage (Average l n)
  | l == 0 = Nothing
  | otherwise = Just $ n / fromIntegral l

instance Num n => Semigroup (Average n) where
  Average lx nx <> Average ly ny = Average (lx + ly) (nx + ny)

instance Num n => Monoid (Average n) where
  mappend = (<>)
  mempty = Average 0 0

