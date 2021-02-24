{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
module Repository.Queries.TS where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Reader (ask)
import           Control.Monad.State  (MonadState, evalState, get, put)
import           Data.Acid            (Query, Update, makeAcidic)
import           Data.Foldable
import           Data.Functor
import           Data.Monoid
import           Data.Semigroup
import qualified Data.Sequence        as S
import qualified Data.Vector          as V
import qualified DataS.IntMap         as IM
import qualified DataS.Map            as M

import           Aggregates
import           Repository.Model

aggTS :: Monoid m =>
        (m -> a)
        -> (TS -> m)
        -> Maybe Tag
        -> (IM.IntMap TagMap -> IM.IntMap TagMap)
        -> TimeseriesDB
        -> a
aggTS get to tg f TimeseriesDB{..} = get $ foldMap' (mapToM to tg data') (f tIx)

mapToM :: Monoid m => (TS -> m) -> Maybe Tag -> V.Vector TS -> M.Map Tag Ix -> m
mapToM toM Nothing d m   = foldMap' (toM . (V.!) d) m
mapToM toM (Just tg) d m = maybe mempty (toM . (V.!) d) (M.lookup tg m)

tsQuery :: Maybe String
        -> Maybe Tag
        -> (IM.IntMap TagMap -> IM.IntMap TagMap)
        -> ExceptionQuery QueryR
tsQuery (Just agg) tg f
  | agg == "avg" = ask >>= handleAgg "Average Failed" . aggTS getAverage (toAvg . value) tg f
  | agg == "sum" = ask <&> toAggR . aggTS getSum (Sum . value) tg f
  | agg == "count" = ask <&> toAggR . aggTS getSum (const $ Sum 1) tg f
  | agg == "min" = ask <&> toAggR . aggTS getMin (Min . value) tg f
  | agg == "max" = ask <&> toAggR . aggTS getMax (Max . value) tg f
  | otherwise = throwError "Illegal aggregation function"
tsQuery Nothing tg f    = ask <&> aggTS getList toCollect tg f <&> toCollR
