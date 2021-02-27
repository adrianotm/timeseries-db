{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE PolyKinds       #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
module Repository.Queries.General where

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

toCollAggR :: a -> AggRes a v
toCollAggR = Left

toTagAggR :: Group Tag v -> AggRes a v
toTagAggR = Right . Left . getGroup

toTSAggR :: Group Timestamp v -> AggRes a v
toTSAggR = Right . Right . getGroup

data GeneralQuery = GeneralQuery { aggQ   :: Maybe Agg
                                 , groupQ :: Maybe GroupBy
                                 , tdb    :: TimeseriesDB
                                 }

type ExceptGQuery = ExceptT String (Reader GeneralQuery)

type GroupEither v = Either (M.Map Tag v) (M.Map Timestamp v)

type AggRes a v = Either a (GroupEither v)

aggGeneral :: Monoid m =>
    (m -> a)
    -> (TS -> m)
    -> ExceptGQuery (AggRes a m)
aggGeneral get to = ask >>= \GeneralQuery{..}
                                 -> case groupQ of
                                         (Just GByTag) -> return $! toTagAggR $ foldMap' (\ts@TS{..} -> toGroup tag $ to ts) (data' tdb)
                                         (Just GByTimestemp) -> return $! toTSAggR $ foldMap' (\ts@TS{..} -> toGroup timestamp $ to ts) (data' tdb)
                                         Nothing -> return $! toCollAggR $ get $ foldMap' to (data' tdb)
                                         (Just IllegalGBy) -> throwE "Illegal 'groupBy' field."

generalQuery :: ExceptGQuery QueryR
generalQuery = ask
            >>= \GeneralQuery{..}
              -> case aggQ of
      (Just AvgAgg) -> aggGeneral getAverage (toAvg . value) >>=
                                either (handleAgg "Average failed")
                                       (return . toAggRG (fromMaybe 0 . getAverage))
      (Just SumAgg) -> aggGeneral getSum (Sum . value) <&> either toAggR (toAggRG getSum)
      (Just CountAgg) ->  aggGeneral getSum (const $ Sum 1) <&> either toAggR (toAggRG getSum)
      (Just MinAgg) ->  aggGeneral getMin (Min . value) <&> either toAggR (toAggRG getMin)
      (Just MaxAgg) ->  aggGeneral getMax (Max . value) <&> either toAggR (toAggRG getMax)
      (Just IllegalAgg) -> throwE "Illegal aggregation function"
      Nothing ->   aggGeneral getList toCollect <&> toCollR . fromLeft DL.empty
