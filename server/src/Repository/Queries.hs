{-# LANGUAGE RecordWildCards #-}
module Repository.Queries where


import           Control.Monad.Reader      (ask)
import           Data.Either               (fromLeft)
import           Data.Functor              ((<&>))
import           Data.Maybe                (fromMaybe)
import           Data.Monoid               (Sum (Sum, getSum))
import           Data.Semigroup            (Max (Max, getMax),
                                            Min (Min, getMin))
import           Repository.Model          (Agg (AvgAgg, CountAgg, CountAgg, MaxAgg, MinAgg, SumAgg),
                                            Ix, QueryModel (..), QueryR,
                                            TS (value),
                                            TimeseriesDB (TimeseriesDB, _data', _sIx, _tIx))
import           Repository.Queries.Shared (AggRes, ExceptQ,
                                            InternalQ (InternalQ, qm, tdb),
                                            QueryType (TSQuery, TagQuery),
                                            getTS, qmToQT, toQRG)
import           Repository.Queries.Tag    (queryTag)
import           Repository.Queries.TS     (queryTS)

import           Aggregates                (getAverage, getCollList, handleAgg,
                                            toAvg, toCollR, toCollect, toQR)

queryF :: Monoid m => QueryModel -> (m -> a) -> (Ix -> m) -> ExceptQ (AggRes a m)
queryF qm = case qmToQT qm of
                TSQuery  -> queryTS
                TagQuery -> queryTag

query :: ExceptQ QueryR
query = ask
    >>= \InternalQ{qm=qm@Q{..},tdb=TimeseriesDB{..}}
        -> let toM to = to . value . getTS _data' in
               case aggFunc of
                    (Just AvgAgg) -> queryF qm getAverage (toM toAvg) >>=
                                                either (handleAgg "Average failed.")
                                                       (return . toQRG (fromMaybe 0 . getAverage) limit)
                    (Just SumAgg) ->  queryF qm getSum (toM Sum) <&> either toQR (toQRG getSum limit)
                    (Just CountAgg) ->  queryF qm getSum (const $ Sum 1) <&> either toQR (toQRG getSum limit)
                    (Just MinAgg) ->  queryF qm getMin (toM Min) <&> either toQR (toQRG getMin limit)
                    (Just MaxAgg) ->  queryF qm getMax (toM Max) <&> either toQR (toQRG getMax limit)
                    Nothing -> queryF qm getCollList (toCollect . getTS _data') <&> toCollR . maybe id take limit . fromLeft []
