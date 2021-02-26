module Aggregates where

import           Control.Monad.Except
import qualified Data.DList           as DL
import           Data.Foldable
import           DataS.Map
import           Repository.Model

newtype Collect n = Collect { getList :: DL.DList n }

toCollect :: a -> Collect a
toCollect a = Collect (DL.singleton a)

instance Semigroup (Collect n) where
  Collect x <> Collect y = Collect $ DL.append x y

instance Monoid (Collect n) where
  mappend=(<>)
  mempty = Collect DL.empty

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

toAggR :: Val -> QueryR
toAggR = QR . Right . Right . AggR

toCollR :: DL.DList TS -> QueryR
toCollR = QR . Left . DL.toList

handleAgg :: Monad m => String -> Maybe Val  -> ExceptT String m QueryR
handleAgg err = maybe (throwError err) (return . toAggR)

newtype GroupTag k v = GroupTag { getGroup :: Map k v }

instance (Semigroup v, Ord k) => Semigroup (GroupTag k v) where
  GroupTag x <> GroupTag y = GroupTag $ unionWith (<>) x y

instance (Semigroup v, Ord k) => Monoid (GroupTag k v) where
  mempty = GroupTag empty
  mappend = (<>)

mapToGroupAgg :: Semigroup v => (v -> Val) -> Map Tag v -> [GroupAggR]
mapToGroupAgg f = foldrWithKey' (\k v -> (:) (GroupAggR (Left k) $ f v)) []

toAggRG :: Semigroup v => (v -> Val) -> Either (Map Tag v) (Map Timestamp v) -> QueryR
toAggRG f (Left m) = QR $ Right $ Left $ foldrWithKey' (\k v -> (:) (GroupAggR (Left k) $ f v)) [] m
toAggRG f (Right m) = QR $ Right $ Left $ foldrWithKey' (\k v -> (:) (GroupAggR (Right k) $ f v)) [] m
