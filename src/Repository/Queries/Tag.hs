{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
module Repository.Queries.Tag where

import           Control.Monad              (join)
import           Control.Monad.Except
import           Control.Monad.Reader       (Reader, ask)
import           Control.Monad.Trans.Except
import           Data.Acid                  (Query, Update, makeAcidic)
import           Data.DList                 as DL
import           Data.Foldable
import           Data.Functor
import           Data.Maybe
import           Data.Monoid
import           Data.Semigroup
import qualified Data.Vector                as V
import qualified DataS.Map                  as M

import           Aggregates                 as A
import           Repository.Model

data TagQuery = TagQ { aggQ :: Maybe Agg
                     , tdb  :: TimeseriesDB
                     , tagQ :: Tag
                     }

type ExceptTagQuery = ExceptT String (Reader TagQuery)

aggTag :: Monoid m =>
    (m -> a)
    -> (TS -> m)
    -> ExceptTagQuery (Maybe a)
aggTag get to = ask <&> \TagQ{..} -> M.lookup tagQ (sIx tdb) <&> get . foldMap' (to . (V.!) (data' tdb))

noDataErr :: Tag -> String
noDataErr tg = "No data for tag " ++ either show show tg

tagQuery :: ExceptTagQuery QueryR
tagQuery = ask
            >>= \TagQ{..}
              -> case aggQ of
    (Just AvgAgg) ->  aggTag getAverage (toAvg . value) >>= handleAgg "Average failed" . join
    (Just SumAgg) ->  aggTag getSum (Sum . value) >>= handleAgg (noDataErr tagQ)
    (Just CountAgg) ->  aggTag getSum (const $ Sum 1) >>= maybe (return $ toAggR 0) (return . toAggR)
    (Just MinAgg) ->  aggTag getMin (Min . value) >>= handleAgg (noDataErr tagQ)
    (Just MaxAgg) ->  aggTag getMax (Max . value) >>= maybe (throwError $ noDataErr tagQ) (return . toAggR)
    (Just IllegalAgg) -> throwE "Illegal aggregation function"
    Nothing -> aggTag getList toCollect <&> toCollR <$> fromMaybe DL.empty
