module DataS.HashMap
   ( module DataS.HashMap
   , M.HashMap
   , M.mapMaybe
   , M.filter
   , M.unionWith
   , M.insertWith
   , M.differenceWith
   , M.foldlWithKey'
   , M.union
   , M.fromList
   , M.fromListWith
   , M.singleton
   , M.empty
   , M.foldl'
   , M.lookup
   , M.keys
   , M.update
   , (M.!)
   ) where

import           Data.HashMap.Strict as M (HashMap, differenceWith, empty,
                                           filter, foldl', foldlWithKey',
                                           foldrWithKey, foldrWithKey',
                                           fromList, fromListWith, insertWith,
                                           keys, lookup, mapMaybe, singleton,
                                           union, unionWith, update, (!))

foldMapWithKey :: Monoid m => (k -> a -> m) -> HashMap k a -> m
foldMapWithKey f = M.foldrWithKey' (\k v acc -> f k v <> acc) mempty
