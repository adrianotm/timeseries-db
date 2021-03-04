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
                          (Just GByTimestemp) -> return $ toTSAggR $! IM.foldMapWithKey' sort (\k -> toGroup k . foldMap' (to . (V.!) _data')) (qmToF qm _tIx)
                          Nothing -> return $ toCollAggR $ get $! IM.foldMap' sort (foldMap' (to . (V.!) _data')) (qmToF qm _tIx)

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
                                              (Just GByTimestemp) -> return $ toTSAggR $! foldMap' (toGroup ts . to . (V.!) _data') m
                                              (Just GByTag) -> throwE "Can't use 'groupBy = tag' with 'tsEq'."
                                              Nothing -> return $ toCollAggR $ get $! foldMap' (to . (V.!) _data') m
