{-# LANGUAGE RecordWildCards #-}
module Repository.Queries.TS where

import           Control.Monad.Reader       (Reader, ask)
import           Control.Monad.Trans.Except
import           Data.Foldable
import           Data.Functor
import qualified Data.Map.Strict            as M
import qualified Data.Vector                as V
import           DataS.DList                as DL
import qualified DataS.HashMap              as HM
import qualified DataS.IntMap               as IM

import           Aggregates
import           Repository.Model
import           Repository.Queries.Shared

queryTS' :: (Monoid v) => (v -> a) -> (TS -> v) -> Maybe (Timestamp, DL.DList Ix) -> ExceptQ (AggRes a v)
queryTS' get to Nothing = ask <&> \InternalQ{qm=qm@Q{..},tdb=TimeseriesDB{..}}
                                    -> case groupBy of
                                          (Just GByTimestamp) -> toTSAggR $ IM.foldMapWithKey limit sort (\ts dl -> toCollect (ts, foldMap' (to . getTS _data') dl)) (qmToF qm _tIx)
                                          _ -> toCollAggR $ get $ IM.foldMap limit sort (DL.foldMap limit (to . getTS _data')) (qmToF qm _tIx)

queryTS' get to (Just (ts, dl)) = ask <&> \InternalQ{qm=qm@Q{..},tdb=TimeseriesDB{..}}
                                             -> case groupBy of
                                                  (Just GByTimestamp) -> toTSAggR $ toCollect (ts, foldMap' (to . getTS _data') dl)
                                                  _ -> toCollAggR $ get $ DL.foldMap limit (to . getTS _data') dl

queryTS :: (Monoid v) => (v -> a) -> (TS -> v) -> ExceptQ (AggRes a v)
queryTS get to = ask >>= \InternalQ{qm=Q{..},tdb=TimeseriesDB{..}}
                         -> case tsEq of
                             Nothing -> queryTS' get to Nothing
                             (Just ts)
                                -> case IM.lookup ts _tIx of
                                      Nothing -> throwE $ noDataErr (Right ts)
                                      (Just dl) -> queryTS' get to (Just (ts, dl))

