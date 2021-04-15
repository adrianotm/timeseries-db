module DataS.IntMap
   ( module DataS.IntMap
   , IM.IntMap
   , IM.map
   , IM.empty
   , IM.null
   , IM.size
   , IM.singleton
   , IM.foldrWithKey
   , IM.differenceWith
   , IM.difference
   , IM.union
   , IM.unionWith
   , IM.fromList
   , IM.fromListWith
   , IM.lookup
   , IM.insert
   , IM.insertWith
   , IM.keys
   , IM.lookupMax
   , IM.lookupMin
   , IM.update
   , IM.delete
   , (IM.!)
   ) where

import           Data.Foldable        (Foldable (foldMap, foldMap'))
import           Data.IntMap.Internal (IntMap (Bin, Nil, Tip), Key, mask,
                                       nomatch, zero)
import qualified Data.IntMap.Strict   as IM
import           Repository.Model     (Agg, Sort (Desc))

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

-- FoldMap in a descending order
foldMapDesc :: Monoid m => (a -> m) -> IM.IntMap a -> m
foldMapDesc f = go
    where go Nil           = mempty
          go (Tip _ v)     = f v
          go (Bin _ m l r) = go r `mappend` go l

-- FoldMapWithKey in a descending order
foldMapWithKeyDesc :: Monoid m => (Key -> a -> m) -> IM.IntMap a -> m
foldMapWithKeyDesc f = go
    where go Nil           = mempty
          go (Tip kx x)    = f kx x
          go (Bin _ m l r) = go r `mappend` go l

-- Choose a foldMap depending on the aggregation and the sort
foldMap :: Monoid m => Maybe Agg -> Maybe Sort -> (a -> m) -> IM.IntMap a -> m
foldMap Nothing (Just Desc) = foldMapDesc
foldMap Nothing _           = Data.Foldable.foldMap
foldMap (Just _) _          = Data.Foldable.foldMap'
{-# INLINE foldMap #-}

-- Choose a foldMap depending on the sort
foldMapWithKey :: Monoid m => Maybe Sort -> (Key -> a -> m) -> IM.IntMap a -> m
foldMapWithKey (Just Desc) = foldMapWithKeyDesc
foldMapWithKey _           = IM.foldMapWithKey
{-# INLINE foldMapWithKey #-}
