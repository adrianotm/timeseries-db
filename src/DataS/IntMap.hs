{-# LANGUAGE BangPatterns #-}
module DataS.IntMap
   ( module DataS.IntMap
   , IM.IntMap
   , IM.foldrWithKey
   , IM.union
   , IM.unionWith
   , IM.fromList
   , IM.lookup
   , IM.empty
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

foldlDesc' :: (a -> b -> a) -> a -> IM.IntMap b -> a
foldlDesc' f z = \t ->
  case t of
    Bin _ m l r -> go (go z r) l
    _           -> go z t
  where
    go !z' Nil          = z'
    go z' (Tip _ x)     = f z' x
    go z' (Bin _ _ l r) = go (go z' r) l

foldlWithKeyDesc' :: (a -> Key -> b -> a) -> a -> IM.IntMap b -> a
foldlWithKeyDesc' f z = \t ->
  case t of
    Bin _ m l r -> go (go z r) l
    _           -> go z t
  where
    go !z' Nil          = z'
    go z' (Tip kx x)    = f z' kx x
    go z' (Bin _ _ l r) = go (go z' r) l

foldMap' :: Monoid m => Maybe Sort -> (a -> m) -> IM.IntMap a -> m
foldMap' (Just Desc) f = foldlDesc' (\acc a -> acc <> f a) mempty
foldMap' _  f          = Data.Foldable.foldMap' f

foldMapWithKey' :: Monoid m => Maybe Sort -> (Key -> a -> m) -> IM.IntMap a -> m
foldMapWithKey' sort f  = fwk sort (\acc k v -> acc <> f k v) mempty
  where fwk (Just Desc) = foldlWithKeyDesc'
        fwk _           = IM.foldlWithKey'
