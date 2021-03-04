{-# LANGUAGE RecordWildCards #-}
module Repository.Queries where

import           Data.DList                 as DL

import           Control.Monad.Except
import           Control.Monad.Reader       (ask)
import           Control.Monad.Trans.Except
import           Data.Either
import           Data.Functor
import           Data.Maybe
import           Data.Monoid
import           Data.Semigroup
import           Repository.Model
import           Repository.Queries.Shared
import           Repository.Queries.Tag
import           Repository.Queries.TS

import           Aggregates
import           Debug.Trace

debug = flip trace

queryF :: Monoid m => QueryModel -> (m -> a) -> (TS -> m) -> ExceptQ (AggRes a m)
queryF qm = case qmToQT qm of
                TSQuery    -> queryTS `debug` "TS"
                TagQuery t -> queryTag t `debug` "TAG"

query :: ExceptQ QueryR
query = ask
    >>= \InternalQ{qm=qm@Q{..}}
        -> case aggFunc of
            (Just AvgAgg) -> queryF qm getAverage (toAvg . value) >>=
                                        either (handleAgg "Average failed")
                                               (return . toAggRG (fromMaybe 0 . getAverage))
            (Just SumAgg) ->  queryF qm getSum (Sum . value) <&> either toAggR (toAggRG getSum)
            (Just CountAgg) ->  queryF qm getSum (const $ Sum 1) <&> either toAggR (toAggRG getSum)
            (Just MinAgg) ->  queryF qm getMin (Min . value) <&> either toAggR (toAggRG getMin)
            (Just MaxAgg) ->  queryF qm getMax (Max . value) <&> either toAggR (toAggRG getMax)
            (Just IllegalAgg) -> throwE "Illegal aggregation function"
            Nothing -> queryF qm getCollList toCollect <&> toCollR . fromLeft []
