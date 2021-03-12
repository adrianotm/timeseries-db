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

import           Data.DList       as DL (DList, append, cons, empty, fromList,
                                         singleton, snoc, toList)
import           Data.Foldable    (Foldable (foldMap, foldMap'))
import           Repository.Model (Agg)

foldMap :: Monoid m => Maybe Agg -> (a -> m) -> DL.DList a -> m
foldMap Nothing  = Data.Foldable.foldMap
foldMap (Just _) = Data.Foldable.foldMap'
