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


queryTS' :: (Monoid v) =>
           (v -> a)
        -> (TS -> v)
        -> ExceptQ (AggRes a v)
queryTS' get to = ask
                  >>= \InternalQ{qm=qm@Q{..},tdb=TimeseriesDB{..}}
                      -> case groupBy of
                          (Just GByTimestamp) -> return $ toTSAggR $ IM.foldMapWithKey' sort (\k v -> toCollect (k, foldMap' (to . getTS _data') v)) (qmToF qm _tIx)
                          _ -> return $ toCollAggR $ get $ IM.foldMap' sort (foldMap' (to . getTS _data')) (qmToF qm _tIx)

queryTS :: (Monoid v) =>
        (v -> a)
        -> (TS -> v)
        -> ExceptQ (AggRes a v)
queryTS get to = ask >>= \InternalQ{qm=Q{..},tdb=TimeseriesDB{..}}
                         -> case tsEq of
                             Nothing -> queryTS' get to
                             (Just ts)
                                -> case IM.lookup ts _tIx of
                                      Nothing -> throwE $ noDataErr (Right ts)
                                      (Just dl)
                                         -> case groupBy of
                                              (Just GByTimestamp) -> return $ toTSAggR $ toCollect (ts, foldMap' (to . getTS _data') dl)
                                              _ -> return $ toCollAggR $ get $ foldMap' (to . getTS _data') dl
