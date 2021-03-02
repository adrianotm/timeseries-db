{-# LANGUAGE RecordWildCards #-}
module Repository.Queries.TS where

import           Control.Monad.Reader       (Reader, ask)
import           Control.Monad.Trans.Except
import           Data.Foldable
import qualified Data.Vector                as V
import qualified DataS.IntMap               as IM
import qualified DataS.Map                  as M

import           Aggregates
import           Repository.Model
import           Repository.Queries.Shared

mapToM :: (Monoid m) =>
  (TS -> m)
  -> Maybe Tag
  -> V.Vector TS
  -> M.Map Tag Ix
  -> m
mapToM toM Nothing d m   = foldMap' (toM . (V.!) d) m
mapToM toM (Just tg) d m = maybe mempty (toM . (V.!) d) (M.lookup tg m)

mapToMG :: (Monoid v) =>
  (TS -> v)
  -> V.Vector TS
  -> M.Map Tag Ix
  -> Group Tag v
mapToMG toM d = M.foldMapWithKey' (\k -> toGroup k . toM . (V.!) d)

aggTS' :: (Monoid v) =>
           (v -> a)
        -> (TS -> v)
        -> ExceptQ (AggRes a v)
aggTS' get to = ask
                  >>= \InternalQ{qm=qm@Q{..},tdb=TimeseriesDB{..}}
                      -> case groupBy of
                          (Just GByTag) -> return $ toTagAggR $! foldMap' (mapToMG to _data' $!) (qmToF qm  _tIx)
                          (Just GByTimestemp) -> return $ toTSAggR $! IM.foldMapWithKey' (\k v -> toGroup k $ mapToM to tagEq _data' $! v) (qmToF qm _tIx)
                          (Just IllegalGBy) -> throwE "Illegal 'groupBy' field."
                          Nothing -> return $ toCollAggR $ get $! foldMap' (mapToM to tagEq _data' $!) (qmToF qm _tIx)

aggTS :: (Monoid v) =>
        (v -> a)
        -> (TS -> v)
        -> ExceptQ (AggRes a v)
aggTS get to = ask >>= \InternalQ{qm=Q{..},tdb=TimeseriesDB{..}}
                         -> case tsEq of
                             Nothing -> aggTS' get to
                             (Just ts)
                                -> case IM.lookup ts _tIx of
                                      Nothing -> throwE "Timestamp not found"
                                      (Just m)
                                         -> case groupBy of
                                              (Just GByTimestemp) -> return $ toTSAggR $! mapToM (toGroup ts . to) Nothing _data' m
                                              (Just GByTag) -> throwE "Can't use 'groupBy = tag' with 'tsEq'."
                                              (Just IllegalGBy) -> throwE "Illegal 'groupBy' field."
                                              Nothing -> return $ toCollAggR $ get $! mapToM to tagEq _data' m
