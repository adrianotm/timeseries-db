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
import           Data.Bool
import           Data.Either
import           Data.Foldable
import           Data.Functor
import           Data.Maybe
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
                       , groupQ      :: Group
                       , transformIM :: IM.IntMap TagMap -> IM.IntMap TagMap
                       , qm          :: QueryModel
                       , tdb         :: TimeseriesDB
                       }

type ExceptTSQuery = ExceptT String (Reader TSQuery)

simpleAgg :: Monoid m =>
           (m -> a)
        -> (TS -> m)
        -> Maybe Tag
        -> (IM.IntMap TagMap -> IM.IntMap TagMap)
        -> TimeseriesDB
        -> a
simpleAgg get to tg f TimeseriesDB{..} = get $ foldMap' (mapToM to tg data') (f tIx)

mapToM :: (Monoid m) =>
  (TS -> m)
  -> Maybe Tag
  -> V.Vector TS
  -> M.Map Tag Ix
  -> m
mapToM toM Nothing d m   = M.foldMapWithKey' (\k v -> toM $ (V.!) d v) m
mapToM toM (Just tg) d m = maybe mempty (toM . (V.!) d) (M.lookup tg m)

mapToMG :: (Monoid v) =>
  (TS -> v)
  -> V.Vector TS
  -> M.Map Tag Ix
  -> GroupTag Tag v
mapToMG toM d = M.foldMapWithKey' (\k v -> GroupTag $ M.singleton k (toM (d V.! v)))

aggTS' :: (Monoid v) =>
           (v -> a)
        -> (TS -> v)
        -> ExceptTSQuery (Either a (M.Map Tag v))
aggTS' get to = ask <&> \TSQuery{..}
                          -> if groupQ then Right $ getGroup $ foldMap' (mapToMG to (data' tdb)) (transformIM $ tIx tdb)
                                       else Left $ simpleAgg get to tagQ transformIM tdb

aggTS :: (Monoid v) =>
        (v -> a)
        -> (TS -> v)
        -> ExceptTSQuery (Either a (M.Map Tag v))
aggTS get to = ask >>= \TSQuery{..}
                        -> case tsQ of
                             Nothing -> aggTS' get to
                             (Just ts) -> maybe (throwE "Timestamp not found")
                                          return
                                          (IM.lookup ts (tIx tdb) <&>
                                            \m -> bool
                                                    (Left $ get $ mapToM to tagQ (data' tdb) m)
                                                    (Right $ getGroup $ mapToMG to (data' tdb) m)
                                                  groupQ
                                          )

tsQuery :: ExceptTSQuery QueryR
tsQuery = ask
            >>= \TSQuery{..}
              -> case aggQ of
      (Just AvgAgg) -> catchE (aggTS getAverage (toAvg . value) >>=
                                either
                                  (handleAgg "Average failed")
                                  (return . toAggRG (fromMaybe 0 . getAverage))
                             )
                      throwE
      (Just SumAgg) -> aggTS getSum (Sum . value) <&> either toAggR (toAggRG getSum)
      (Just CountAgg) ->  aggTS getSum (const $ Sum 1) <&> either toAggR (toAggRG getSum)
      (Just MinAgg) ->  aggTS getMin (Min . value) <&> either toAggR (toAggRG getMin)
      (Just MaxAgg) ->  aggTS getMax (Max . value) <&> either toAggR (toAggRG getMax)
      (Just IllegalAgg) -> throwE "Illegal aggregation function"
      Nothing ->  return $ toCollR $ simpleAgg getList toCollect tagQ transformIM tdb
