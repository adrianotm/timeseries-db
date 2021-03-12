{-# LANGUAGE RecordWildCards #-}
module Repository.Queries.TS where

import           Control.Monad.Reader       (Reader, ask)
import           Control.Monad.Trans.Except
import           Data.Foldable
import           Data.Functor
import qualified Data.Map.Strict            as M
import qualified Data.Vector                as V
import qualified DataS.DList                as DL
import qualified DataS.IntMap               as IM

import           Aggregates
import           Model
import           Repository.Model
import           Repository.Queries.Shared

queryTS' :: (Monoid v) => (v -> a) -> (Ix -> v) -> Maybe (Timestamp, DL.DList Ix) -> ExceptQ (AggRes a v)
queryTS' get to Nothing = ask <&> \InternalQ{qm=qm@Q{..},tdb=TimeseriesDB{..}}
                                    -> case groupBy of
                                          (Just GByTimestamp) -> toTSAggR $ IM.foldMapWithKey sort (\ts dl -> toCollect (ts, foldMap' to dl)) (qmToF qm _tIx)
                                          _ -> toCollAggR $ get $ IM.foldMap aggFunc sort (DL.foldMap aggFunc to) (qmToF qm _tIx)

queryTS' get to (Just (ts, dl)) = ask <&> \InternalQ{qm=qm@Q{..},tdb=TimeseriesDB{..}}
                                             -> case groupBy of
                                                  (Just GByTimestamp) -> toTSAggR $ toCollect (ts, foldMap' to dl)
                                                  _ -> toCollAggR $ get $ DL.foldMap aggFunc to dl

queryTS :: (Monoid v) => (v -> a) -> (Ix -> v) -> ExceptQ (AggRes a v)
queryTS get to = ask >>= \InternalQ{qm=Q{..},tdb=TimeseriesDB{..}}
                         -> case tsEq of
                             Nothing -> queryTS' get to Nothing
                             (Just ts)
                                -> case IM.lookup ts _tIx of
                                      Nothing -> throwE $ noDataErr (Right ts)
                                      (Just dl) -> queryTS' get to (Just (ts, dl))

