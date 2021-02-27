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
import           Data.DList                 as DL
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
                       , groupQ      :: Maybe GroupBy
                       , transformIM :: IM.IntMap TagMap -> IM.IntMap TagMap
                       , tdb         :: TimeseriesDB
                       }

type GroupEither v = Either (M.Map Tag v) (M.Map Timestamp v)

type AggRes a v = Either a (GroupEither v)

type ExceptTSQuery = ExceptT String (Reader TSQuery)

toCollAggR :: a -> AggRes a v
toCollAggR = Left

toTagAggR :: Group Tag v -> AggRes a v
toTagAggR = Right . Left . getGroup

toTSAggR :: Group Timestamp v -> AggRes a v
toTSAggR = Right . Right . getGroup

mapToM :: (Monoid m) =>
  (TS -> m)
  -> Maybe Tag
  -> V.Vector TS
  -> M.Map Tag Ix
  -> m
mapToM toM Nothing d m   = foldMap' (toM . (V.!) d) m
mapToM toM (Just tg) d m = maybe mempty (toM . (V.!) d) (M.lookup tg m)

mapToMG :: (Monoid v) =>
  (TS -> v)
  -> V.Vector TS
  -> M.Map Tag Ix
  -> Group Tag v
mapToMG toM d = M.foldMapWithKey' (\k -> toGroup k . toM . (V.!) d)

aggTS' :: (Monoid v) =>
           (v -> a)
        -> (TS -> v)
        -> ExceptTSQuery (AggRes a v)
aggTS' get to = ask
                  >>= \TSQuery{..}
                      -> case groupQ of
                          (Just GByTag) -> return $ toTagAggR $! foldMap' (mapToMG to (data' tdb)) (transformIM $ tIx tdb)
                          (Just GByTimestemp) -> return $ toTSAggR $! IM.foldMapWithKey' (\k -> toGroup k . mapToM to tagQ (data' tdb)) (transformIM $ tIx tdb)
                          (Just IllegalGBy) -> throwE "Illegal 'groupBy' field."
                          Nothing -> return $ toCollAggR $ get $! foldMap' (mapToM to tagQ $ data' tdb) (transformIM $ tIx tdb)

aggTS :: (Monoid v) =>
        (v -> a)
        -> (TS -> v)
        -> ExceptTSQuery (AggRes a v)
aggTS get to = ask >>= \TSQuery{..}
                         -> case tsQ of
                             Nothing -> aggTS' get to
                             (Just ts)
                                -> case IM.lookup ts (tIx tdb) of
                                      Nothing -> throwE "Timestamp not found"
                                      (Just m)
                                         -> case groupQ of
                                              (Just GByTimestemp) -> return $ toTSAggR $! mapToM (toGroup ts . to) Nothing (data' tdb) m
                                              (Just GByTag) -> throwE "Can't use 'groupBy = tag' with 'tsEq'."
                                              (Just IllegalGBy) -> throwE "Illegal 'groupBy' field."
                                              Nothing -> return $ toCollAggR $ get $! mapToM to tagQ (data' tdb) m

tsQuery :: ExceptTSQuery QueryR
tsQuery = ask
            >>= \TSQuery{..}
              -> case aggQ of
      (Just AvgAgg) -> aggTS getAverage (toAvg . value) >>=
                                either (handleAgg "Average failed")
                                       (return . toAggRG (fromMaybe 0 . getAverage))
      (Just SumAgg) -> aggTS getSum (Sum . value) <&> either toAggR (toAggRG getSum)
      (Just CountAgg) ->  aggTS getSum (const $ Sum 1) <&> either toAggR (toAggRG getSum)
      (Just MinAgg) ->  aggTS getMin (Min . value) <&> either toAggR (toAggRG getMin)
      (Just MaxAgg) ->  aggTS getMax (Max . value) <&> either toAggR (toAggRG getMax)
      (Just IllegalAgg) -> throwE "Illegal aggregation function"
      Nothing ->   aggTS getList toCollect <&> toCollR . fromLeft DL.empty
