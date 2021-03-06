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
import qualified Data.IntMap.Strict   as IM
import           Repository.Model


--- Bool - return the equal key
lookupGT' :: Bool -> Key -> IM.IntMap a -> IM.IntMap a
lookupGT' re k im =
   case im of
     Bin p m l r
       | nomatch k p m -> if mask k m > p then Nil else im
       | zero k m -> Bin p m (lookupGT' re k l) r
       | otherwise -> r
     t@(Tip ky y)
       | k < ky -> t
       | re && k == ky -> t
       | otherwise -> Nil
     Nil -> Nil

--- Bool - return the equal key
lookupLT' :: Bool -> Key -> IM.IntMap a -> IM.IntMap a
lookupLT' re k im =
   case im of
     Bin p m l r
       | nomatch k p m -> if mask k m < p then Nil else im
       | zero k m -> lookupLT' re k l
       | otherwise -> Bin p m l $ lookupLT' re k r
     t@(Tip ky y)
       | k > ky -> t
       | re && k == ky -> t
       | otherwise -> Nil
     Nil -> Nil


--- Bool - return the equal key
lookupGLT' :: Bool -> Bool -> Key -> Key -> IM.IntMap a -> IM.IntMap a
lookupGLT' re1 re2 k1 k2 im =
   case im of
     Bin p m l r
       | mask k1 m <= p && p <= mask k2 m -> Bin p m (lookupGLT' re1 re2 k1 k2 l) (lookupGLT' re1 re2 k1 k2 r)
       | otherwise -> Nil
     t@(Tip ky y)
       | k1 < ky && ky < k2 -> t
       | re1 && ky < k2 && k1 == ky -> t
       | k1 < ky && re2 && k2 == ky -> t
       | otherwise -> Nil
     Nil -> Nil

foldMap' :: Monoid m => Maybe Sort -> (a -> m) -> IM.IntMap a -> m
foldMap' (Just Desc) f = IM.foldr' (\a acc -> acc <> f a) mempty
foldMap' _  f          = Data.Foldable.foldMap' f

foldMapWithKey' :: Monoid m => Maybe Sort -> (Key -> a -> m) -> IM.IntMap a -> m
foldMapWithKey' (Just Desc) f  = IM.foldrWithKey' (\k v acc -> acc <> f k v) mempty
foldMapWithKey' _ f = IM.foldlWithKey' (\acc k v -> acc <> f k v) mempty
