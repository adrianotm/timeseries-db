{-# LANGUAGE RecordWildCards #-}

module Repository.Queries.TS (queryTS) where

import           Control.Monad.Reader        (Reader, ask)
import           Control.Monad.Trans.Except  (throwE)
import           Control.Parallel.Strategies
import           Data.Bifunctor              (second)
import           Data.Foldable               (Foldable (foldMap, foldMap'))
import           Data.Functor                ((<&>))
import qualified Data.Vector                 as V
import qualified DataS.IntMap                as IM
import           Repository.Model            (Agg, GroupBy (..), Ix,
                                              QueryModel (..), Tag,
                                              TimeseriesDB (..), Timestamp)
import           Repository.Queries.Utils    (AggRes, ExceptQ, InternalQ (..),
                                              noDataErr, qmToF, toCollAggR,
                                              toTSAggR, toTagAggR)

-- | Use a strict foldMap when a 'aggFunc' is present in the query
foldMapL :: Monoid m => Maybe Agg -> (a -> m) -> [a] -> m
foldMapL Nothing  = Data.Foldable.foldMap
foldMapL (Just _) = Data.Foldable.foldMap'
{-# INLINE foldMapL #-}

-- | Helper function to query the timestamp index
-- if a 'groupBy' is present, aggregate each group in parallel
-- otherwise aggregate the whole data with a Monoid
--
-- 'groupBy = "tag"' can't exist because the queryTag function would have been picked
queryTS' :: (NFData m, Monoid m) => (m -> a) -> (Ix -> m) -> Maybe (Timestamp, [Ix]) -> ExceptQ (AggRes a m)
queryTS' get to Nothing =
  ask <&> \InternalQ {qm = qm@Q {..}, tdb = TimeseriesDB {..}} ->
    case groupBy of
      (Just GByTimestamp) ->
        toTSAggR
          ( map
              (second (foldMap' to))
              ( IM.toList sort $
                  qmToF qm _tIx
              )
              `using` parBuffer 300 rdeepseq
          )
      _ -> toCollAggR $ get $ IM.foldMap aggFunc sort (foldMapL aggFunc to) (qmToF qm _tIx)
queryTS' get to (Just (ts, ixs)) =
  ask <&> \InternalQ {qm = qm@Q {..}, tdb = TimeseriesDB {..}} ->
    case groupBy of
      (Just GByTimestamp) -> toTSAggR [(ts, foldMap' to ixs)]
      _                   -> toCollAggR $ get $ foldMapL aggFunc to ixs

-- | Query by the timestamp index i.e. the IntMap
-- Check whether a 'tsEq' is present in the query:
-- if it isn't, pass Nothing as third third argument of the helper function
-- else if no data exists for that timestamp, throw an error
-- else pass the data for that timestamp to the helper function
-- return the aggregation result
queryTS :: (NFData m, Monoid m) => (m -> a) -> (Ix -> m) -> ExceptQ (AggRes a m)
queryTS get to =
  ask >>= \InternalQ {qm = Q {..}, tdb = TimeseriesDB {..}} ->
    case tsEq of
      Nothing -> queryTS' get to Nothing
      (Just ts) ->
        case IM.lookup ts _tIx of
          Nothing    -> throwE $ noDataErr (Right ts)
          (Just ixs) -> queryTS' get to (Just (ts, ixs))
