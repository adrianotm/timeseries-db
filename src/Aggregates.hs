module Aggregates where

import           Control.Monad.Except
import qualified Data.DList           as DL
import           Data.Foldable
import qualified Data.Set             as S
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

getCollList :: Collect n -> [n]
getCollList = DL.toList . getList

---------------------------
newtype OrdCollect n = OrdCollect { getOrdCollect :: S.Set n }

toOrdCollect :: a -> OrdCollect a
toOrdCollect a = OrdCollect (S.singleton a)

instance Ord n => Semigroup (OrdCollect n) where
  OrdCollect x <> OrdCollect y = OrdCollect $ S.union x y

instance Ord n => Monoid (OrdCollect n) where
  mempty = OrdCollect S.empty
  mappend = (<>)

getOrdList :: OrdCollect n -> [n]
getOrdList = S.toList . getOrdCollect

---------------------------
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

toAggR :: Val -> QueryR
toAggR = QR . Right . Right . AggR

toCollR :: [TS] -> QueryR
toCollR = QR . Left

handleAgg :: Monad m => String -> Maybe Val  -> ExceptT String m QueryR
handleAgg err = maybe (throwError err) (return . toAggR)
