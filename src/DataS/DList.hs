module DataS.DList
   ( module DataS.DList
   , DL.DList
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

foldMap :: Monoid m => Maybe Limit -> (a -> m) -> DList a -> m
foldMap Nothing  = Data.Foldable.foldMap'
foldMap (Just _) = Data.Foldable.foldMap

