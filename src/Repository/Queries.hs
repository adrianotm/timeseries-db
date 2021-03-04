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

queryF :: Monoid m => QueryModel -> (m -> a) -> (TS -> m) -> ExceptQ (AggRes a m)
queryF qm = case qmToQT qm of
                TSQuery    -> queryTS
                TagQuery t -> queryTag t

query :: ExceptQ QueryR
query = ask
    >>= \InternalQ{qm=qm@Q{..}}
        -> case aggFunc of
            (Just AvgAgg) -> queryF qm getAverage (toAvg . value) >>=
                                        either (handleAgg "Average failed")
                                               (return . toAggRG (fromMaybe 0 . getAverage) sort)
            (Just SumAgg) ->  queryF qm getSum (Sum . value) <&> either toAggR (toAggRG getSum sort)
            (Just CountAgg) ->  queryF qm getSum (const $ Sum 1) <&> either toAggR (toAggRG getSum sort)
            (Just MinAgg) ->  queryF qm getMin (Min . value) <&> either toAggR (toAggRG getMin sort)
            (Just MaxAgg) ->  queryF qm getMax (Max . value) <&> either toAggR (toAggRG getMax sort)
            Nothing -> queryF qm getCollList toCollect <&> toCollR . fromLeft []
