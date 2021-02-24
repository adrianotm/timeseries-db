{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE PolyKinds       #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
module Repository.Queries.TS where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Reader       (Reader, ask)
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

data TSQuery = TSQuery { tagQ        :: Maybe Tag
                       , aggQ        :: Maybe Agg
                       , tsQ         :: Maybe Timestamp
                       , transformIM :: IM.IntMap TagMap -> IM.IntMap TagMap
                       , qm          :: QueryModel
                       , tdb         :: TimeseriesDB
                       }

type ExceptTSQuery = ExceptT String (Reader TSQuery)

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
        -> ExceptTSQuery a
aggTS get to = ask >>= \TSQuery{..} -> case tsQ of
                                         Nothing -> return $ aggTS' get to tagQ transformIM tdb
                                         (Just ts) -> maybe (throwE "Timestamp not found")
                                                      return
                                                      (IM.lookup ts (tIx tdb) <&> get . mapToM to tagQ (data' tdb))

mapToM :: Monoid m => (TS -> m) -> Maybe Tag -> V.Vector TS -> M.Map Tag Ix -> m
mapToM toM Nothing d m   = foldMap' (toM . (V.!) d) m
mapToM toM (Just tg) d m = maybe mempty (toM . (V.!) d) (M.lookup tg m)

tsQuery :: ExceptTSQuery QueryR
tsQuery = ask
            >>= \TSQuery{..}
              -> case aggQ of
                (Just "avg") -> catchE (aggTS getAverage (toAvg . value) >>= handleAgg "Average failed") throwE
                (Just "sum") -> (<&> toAggR) $ aggTS getSum (Sum . value)
                (Just "count") -> (<&> toAggR) $ aggTS getSum (const $ Sum 1)
                (Just "min") -> (<&> toAggR) $ aggTS getMin (Min . value)
                (Just "max") -> (<&> toAggR) $ aggTS getMax (Max . value)
                (Just _) -> throwE "Illegal aggregation function"
                Nothing -> (<&> toCollR) $ aggTS getList toCollect
