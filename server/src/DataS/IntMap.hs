module DataS.IntMap
   ( module DataS.IntMap
   , IM.IntMap
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
import           Model

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

foldMapDesc :: Monoid m => (a -> m) -> IM.IntMap a -> m
foldMapDesc f = go
    where go Nil           = mempty
          go (Tip _ v)     = f v
          go (Bin _ m l r) = go r `mappend` go l

foldMapWithKeyDesc :: Monoid m => (Key -> a -> m) -> IM.IntMap a -> m
foldMapWithKeyDesc f = go
    where go Nil           = mempty
          go (Tip kx x)    = f kx x
          go (Bin _ m l r) = go r `mappend` go l

foldMap :: Monoid m => Maybe Agg -> Maybe Sort -> (a -> m) -> IM.IntMap a -> m
foldMap Nothing (Just Desc) = foldMapDesc
foldMap Nothing _           = Data.Foldable.foldMap
foldMap (Just _) _          = Data.Foldable.foldMap'

foldMapWithKey :: Monoid m => Maybe Sort -> (Key -> a -> m) -> IM.IntMap a -> m
foldMapWithKey (Just Desc) = foldMapWithKeyDesc
foldMapWithKey _           = IM.foldMapWithKey
