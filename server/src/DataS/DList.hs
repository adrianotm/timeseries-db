module DataS.DList
   ( module DataS.DList
   , DL.DList
   , DL.cons
   , DL.singleton
   , DL.append
   , DL.empty
   , DL.toList
   , DL.fromList
   , DL.snoc
   )
   where

import           Data.DList       as DL
import           Data.Foldable
import           Repository.Model

foldMap :: Monoid m => Maybe Agg -> (a -> m) -> DL.DList a -> m
foldMap Nothing  = Data.Foldable.foldMap
foldMap (Just _) = Data.Foldable.foldMap'
