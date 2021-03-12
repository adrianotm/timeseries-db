{-# LANGUAGE RecordWildCards #-}
module Repository.Queries where

import           DataS.DList                as DL

import           Control.Monad.Except
import           Control.Monad.Reader       (ask)
import           Control.Monad.Trans.Except
import           Data.Either
import           Data.Functor
import           Data.Maybe
import           Data.Monoid
import           Data.Semigroup
import           Model
import           Repository.Queries.Shared
import           Repository.Queries.Tag
import           Repository.Queries.TS

import           Aggregates
import           Repository.Model

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
