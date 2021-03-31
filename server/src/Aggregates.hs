module Aggregates where

import           Control.Monad.Except (ExceptT, MonadError (throwError))
import           Data.Map.Strict      (Map, empty, singleton, unionWith)
import           Repository.Model     (AggR (AggR), QueryR (..), TS, Val)

data Average n = Average { length :: !Int, sum :: !n }

toAvg :: a -> Average a
toAvg = Average 1
{-# INLINE toAvg #-}

getAverage :: (Num n, Fractional n) => Average n -> Maybe n
getAverage (Average l n) | l == 0 = Nothing
                         | otherwise = Just $ n / fromIntegral l

instance Num n => Semigroup (Average n) where
  Average lx nx <> Average ly ny = Average (lx + ly) (nx + ny)

instance Num n => Monoid (Average n) where
  mappend = (<>)
  mempty = Average 0 0

---------------------------
newtype Group k v = Group { getGroup :: Map k v }

instance (Semigroup v, Ord k) => Semigroup (Group k v) where
  Group x <> Group y = Group $ unionWith (<>) x y

instance (Semigroup v, Ord k) => Monoid (Group k v) where
  mempty = Group empty
  mappend = (<>)

--------------------------
toGroup :: k -> v -> Group k v
toGroup k v = Group $ singleton k v
{-# INLINE toGroup #-}

toQR :: Val -> QueryR
toQR = QR . Right . Right . AggR
{-# INLINE toQR #-}

toCollR :: [TS] -> QueryR
toCollR = QR . Left
{-# INLINE toCollR #-}

handleAgg :: Monad m => String -> Maybe Val  -> ExceptT String m QueryR
handleAgg err = maybe (throwError err) (return . toQR)
{-# INLINE handleAgg #-}
