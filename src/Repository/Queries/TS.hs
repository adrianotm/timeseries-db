{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
module Repository.Queries.TS where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Reader       (ask)
import           Control.Monad.State        (MonadState, evalState, get, put)
import           Control.Monad.Trans.Except
import           Data.Acid                  (Query, Update, makeAcidic)
import           Data.Foldable
import           Data.Functor
import           Data.Monoid
import           Data.Semigroup
import qualified Data.Sequence              as S
import qualified Data.Vector                as V
import qualified DataS.IntMap               as IM
import qualified DataS.Map                  as M

import           Aggregates
import           Repository.Model

aggTS' :: Monoid m =>
        (m -> a)
        -> (TS -> m)
        -> Maybe Tag
        -> (IM.IntMap TagMap -> IM.IntMap TagMap)
        -> TimeseriesDB
        -> a
aggTS' get to tg f TimeseriesDB{..} = get $ foldMap' (mapToM to tg data') (f tIx)

aggTS :: Monoid m =>
        (m -> a)
        -> (TS -> m)
        -> Maybe Tag
        -> Maybe Timestamp
        -> (IM.IntMap TagMap -> IM.IntMap TagMap)
        -> TimeseriesDB
        -> ExceptionQuery a
aggTS get to tg Nothing f db = return $ aggTS' get to tg f db
aggTS get to tg (Just ts) f TimeseriesDB{..} = maybe (throwE "Timestamp not found")
                                                     return
                                                     (IM.lookup ts tIx <&> get . mapToM to tg data')

mapToM :: Monoid m => (TS -> m) -> Maybe Tag -> V.Vector TS -> M.Map Tag Ix -> m
mapToM toM Nothing d m   = foldMap' (toM . (V.!) d) m
mapToM toM (Just tg) d m = maybe mempty (toM . (V.!) d) (M.lookup tg m)

tsQuery :: Maybe Agg
        -> Maybe Timestamp
        -> Maybe Tag
        -> (IM.IntMap TagMap -> IM.IntMap TagMap)
        -> ExceptionQuery QueryR
tsQuery (Just agg) ts tg f
  | agg == "avg" = ask >>= \d -> catchE (aggTS getAverage (toAvg . value) tg ts f d >>= handleAgg "Average failed") throwE
  | agg == "sum" = ask >>= (<&> toAggR) . aggTS getSum (Sum . value) tg ts f
  | agg == "count" = ask >>= (<&> toAggR) . aggTS getSum (const $ Sum 1) tg ts f
  | agg == "min" = ask >>= (<&> toAggR) . aggTS getMin (Min . value) tg ts f
  | agg == "max" = ask >>= (<&> toAggR) . aggTS getMax (Max . value) tg ts f
  | otherwise = throwE "Illegal aggregation function"
tsQuery Nothing ts tg f    = ask >>= (<&> toCollR) . aggTS getList toCollect tg ts f
