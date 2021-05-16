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
                                              QueryModel (..), TS (..), Tag,
                                              TimeseriesDB (..), Timestamp)
import           Repository.Queries.Utils    (AggRes, ExceptQ, InternalQ (..),
                                              noDataErr, qmToF, toCollAggR,
                                              toTSAggR, toTagAggR)

foldMapL :: Monoid m => Maybe Agg -> (a -> m) -> [a] -> m
foldMapL Nothing  = Data.Foldable.foldMap
foldMapL (Just _) = Data.Foldable.foldMap'
{-# INLINE foldMapL #-}

queryTS' :: (NFData m, Monoid m) => (m -> a) -> (TS -> m) -> Maybe (Timestamp, [TS]) -> ExceptQ (AggRes a m)
queryTS' get to Nothing =
  ask <&> \InternalQ {qm = qm@Q {..}, tdb = TimeseriesDB {..}} ->
    case groupBy of
      (Just GByTimestamp) ->
        toTSAggR
          ( map
              (second (foldMap' to))
              ( IM.toList sort $
                  qmToF qm tIx
              )
              `using` parBuffer 300 rdeepseq
          )
      _ -> toCollAggR $ get $ IM.foldMap aggFunc sort (foldMapL aggFunc to) (qmToF qm tIx)
queryTS' get to (Just (ts, ixs)) =
  ask <&> \InternalQ {qm = qm@Q {..}, tdb = TimeseriesDB {..}} ->
    case groupBy of
      (Just GByTimestamp) -> toTSAggR [(ts, foldMap' to ixs)]
      _                   -> toCollAggR $ get $ foldMapL aggFunc to ixs

-- Query by the timestamp index
queryTS :: (NFData m, Monoid m) => (m -> a) -> (TS -> m) -> ExceptQ (AggRes a m)
queryTS get to =
  ask >>= \InternalQ {qm = Q {..}, tdb = TimeseriesDB {..}} ->
    case tsEq of
      Nothing -> queryTS' get to Nothing
      (Just ts) ->
        case IM.lookup ts tIx of
          Nothing    -> throwE $ noDataErr (Right ts)
          (Just ixs) -> queryTS' get to (Just (ts, ixs))
