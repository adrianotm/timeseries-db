module DataS.IntMap
   ( module DataS.IntMap
   , IM.IntMap
   , IM.empty
   , IM.singleton
   , IM.foldrWithKey
   , IM.differenceWith
   , IM.difference
   , IM.union
   , IM.unionWith
   , IM.fromList
   , IM.fromListWith
   , IM.lookup
   , IM.insertWith
   , IM.keys
   , IM.lookupMax
   , IM.lookupMin
   , (IM.!)
   ) where

import           Control.Monad.State
import           Data.Foldable
import qualified Data.HashMap.Strict  as HM
import           Data.IntMap.Internal
import qualified Data.IntMap.Lazy     as IML
import qualified Data.IntMap.Strict   as IM
import           Repository.Model

--- Bool - return the equal key
getGT :: Bool -> Key -> IM.IntMap a -> IM.IntMap a
getGT re k im =
   case im of
     Bin p m l r
       | nomatch k p m -> if mask k m > p then Nil else im
       | zero k m -> Bin p m (getGT re k l) r
       | otherwise -> getGT re k r
     t@(Tip ky y)
       | k < ky -> t
       | re && k == ky -> t
       | otherwise -> Nil
     Nil -> Nil

--- Bool - return the equal key
getLT :: Bool -> Key -> IM.IntMap a -> IM.IntMap a
getLT re k im =
   case im of
     Bin p m l r
       | nomatch k p m -> if mask k m < p then Nil else im
       | zero k m -> getLT re k l
       | otherwise -> Bin p m l $ getLT re k r
     t@(Tip ky y)
       | k > ky -> t
       | re && k == ky -> t
       | otherwise -> Nil
     Nil -> Nil

--- Bool - return the equal key
getGLT :: Bool -> Bool -> Key -> Key -> IM.IntMap a -> IM.IntMap a
getGLT re1 re2 k1 k2 im =
   case im of
     Bin p m l r
       | mask k1 m <= p && p <= mask k2 m -> Bin p m (getGLT re1 re2 k1 k2 l) (getGLT re1 re2 k1 k2 r)
       | otherwise -> Nil
     t@(Tip ky y)
       | k1 < ky && ky < k2 -> t
       | re1 && ky < k2 && k1 == ky -> t
       | k1 < ky && re2 && k2 == ky -> t
       | otherwise -> Nil
     Nil -> Nil

foldMap :: Monoid m => Maybe Limit -> Maybe Sort -> (a -> m) -> IM.IntMap a -> m
foldMap Nothing (Just Desc) f  = IM.foldr' (\a acc -> acc <> f a) mempty
foldMap Nothing _  f           = Data.Foldable.foldMap' f
foldMap (Just _) (Just Desc) f = IM.foldl (\acc a -> f a <> acc) mempty
foldMap (Just _) _ f           = Data.Foldable.foldMap f

foldMapWithKey :: Monoid m => Maybe Limit -> Maybe Sort -> (Key -> a -> m) -> IM.IntMap a -> m
foldMapWithKey Nothing (Just Desc) f  = IM.foldrWithKey' (\k v acc -> acc <> f k v) mempty
foldMapWithKey Nothing _ f = IM.foldlWithKey' (\acc k v -> acc <> f k v) mempty
foldMapWithKey (Just _) (Just Desc) f = IML.foldlWithKey (\acc k v -> f k v <> acc) mempty
foldMapWithKey (Just _) _ f = IML.foldrWithKey (\k v acc -> f k v <> acc) mempty
