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
import           Repository.Queries.General
import           Repository.Queries.Shared
import           Repository.Queries.Tag
import           Repository.Queries.TS

import           Aggregates

type AggFunc a m = (m -> a) -> (TS -> m) -> ExceptQ (AggRes a m)

aggF :: Monoid m => QueryModel -> AggFunc a m
aggF Q {gt = (Just _)}    = aggTS
aggF Q {ge = (Just _)}    = aggTS
aggF Q {lt = (Just _)}    = aggTS
aggF Q {le = (Just _)}    = aggTS
aggF Q {tsEq = (Just _)}  = aggTS
aggF Q {tagEq = (Just t)} = aggTag t
aggF _                    = aggGeneral

query :: ExceptQ QueryR
query = ask
    >>= \InternalQ{qm=qm@Q{..}, ..}
        -> case aggFunc of
            (Just AvgAgg) -> aggF qm getAverage (toAvg . value) >>=
                                        either (handleAgg "Average failed")
                                               (return . toAggRG (fromMaybe 0 . getAverage))
            (Just SumAgg) ->  aggF qm getSum (Sum . value) <&> either toAggR (toAggRG getSum)
            (Just CountAgg) ->  aggF qm getSum (const $ Sum 1) <&> either toAggR (toAggRG getSum)
            (Just MinAgg) ->  aggF qm getMin (Min . value) <&> either toAggR (toAggRG getMin)
            (Just MaxAgg) ->  aggF qm getMax (Max . value) <&> either toAggR (toAggRG getMax)
            (Just IllegalAgg) -> throwE "Illegal aggregation function"
            Nothing -> aggF qm getList toCollect <&> toCollR . fromLeft DL.empty
