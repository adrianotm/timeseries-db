module DataS.HashMap
   ( module DataS.HashMap
   , M.HashMap
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
   , (M.!)
   ) where

import           Data.HashMap.Strict as M
import           Repository.Model

foldMapWithKey :: Monoid m => (k -> a -> m) -> HashMap k a -> m
foldMapWithKey f = M.foldrWithKey (\k v acc -> f k v <> acc) mempty
