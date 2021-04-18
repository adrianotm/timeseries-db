module DataS.HashMap
  ( module DataS.HashMap,
    M.HashMap,
    M.traverseWithKey,
    M.mapMaybe,
    M.filter,
    M.unionWith,
    M.insertWith,
    M.insert,
    M.differenceWith,
    M.foldlWithKey',
    M.union,
    M.fromList,
    M.fromListWith,
    M.singleton,
    M.empty,
    M.foldl',
    M.lookup,
    M.keys,
    M.update,
    M.toList,
    M.member,
    (M.!),
  )
where

import qualified Data.HashMap.Strict as M

foldMapWithKey :: Monoid m => (k -> a -> m) -> M.HashMap k a -> m
foldMapWithKey f = M.foldrWithKey' (\k v acc -> f k v <> acc) mempty
