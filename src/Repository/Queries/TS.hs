{-# LANGUAGE RecordWildCards #-}
module Repository.Queries.TS where

import           Control.Monad.Reader       (Reader, ask)
import           Control.Monad.Trans.Except
import           Data.DList                 as DL
import           Data.Foldable
import qualified Data.Map.Strict            as M
import qualified Data.Vector                as V
import qualified DataS.HashMap              as HM
import qualified DataS.IntMap               as IM

import           Aggregates
import           Repository.Model
import           Repository.Queries.Shared

mapToM :: (Monoid m) =>
  (TS -> m)
  -> V.Vector TS
  -> DL.DList Ix
  -> m
mapToM toM d = foldMap' (toM . (V.!) d)

mapToMG :: (Monoid v) =>
  (TS -> v)
  -> V.Vector TS
  -> DL.DList Ix
  -> Group Tag v
mapToMG toM d = foldMap' (\ix -> let ts@TS{..} = (V.!) d ix in toGroup tag $ toM ts)

queryTS' :: (Monoid v) =>
           (v -> a)
        -> (TS -> v)
        -> ExceptQ (AggRes a v)
queryTS' get to = ask
                  >>= \InternalQ{qm=qm@Q{..},tdb=TimeseriesDB{..}}
                      -> case groupBy of
                          (Just GByTag) -> return $ toTagAggR $! foldMap' (mapToMG to _data' $!) (qmToF qm  _tIx)
                          (Just GByTimestemp) -> return $ toTSAggR $! IM.foldMapWithKey' (\k -> toGroup k . mapToM to _data') (qmToF qm _tIx)
                          (Just IllegalGBy) -> throwE "Illegal 'groupBy' field."
                          Nothing -> return $ toCollAggR $ get $! foldMap' (mapToM to _data' $!) (qmToF qm _tIx)

queryTS :: (Monoid v) =>
        (v -> a)
        -> (TS -> v)
        -> ExceptQ (AggRes a v)
queryTS get to = ask >>= \InternalQ{qm=Q{..},tdb=TimeseriesDB{..}}
                         -> case tsEq of
                             Nothing -> queryTS' get to
                             (Just ts)
                                -> case IM.lookup ts _tIx of
                                      Nothing -> throwE "Timestamp not found."
                                      (Just m)
                                         -> case groupBy of
                                              (Just GByTimestemp) -> return $ toTSAggR $! mapToM (toGroup ts . to) _data' m
                                              (Just GByTag) -> throwE "Can't use 'groupBy = tag' with 'tsEq'."
                                              (Just IllegalGBy) -> throwE "Illegal 'groupBy' field."
                                              Nothing -> return $ toCollAggR $ get $! mapToM to _data' m
