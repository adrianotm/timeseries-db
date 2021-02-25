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
import           Data.Foldable
import           Data.Functor
import           Data.Maybe
import           Data.Monoid
import           Data.Semigroup
import           Data.Sequence              as S
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
    (Just "avg") ->  aggTag getAverage (toAvg . value) >>= handleAgg "Average failed" . join
    (Just "sum") ->  aggTag getSum (Sum . value) >>= handleAgg (noDataErr tagQ)
    (Just "count") ->  aggTag getSum (const $ Sum 1) >>= maybe (return $ toAggR 0) (return . toAggR)
    (Just "min") ->  aggTag getMin (Min . value) >>= handleAgg (noDataErr tagQ)
    (Just "max") ->  aggTag getMax (Max . value) >>= maybe (throwError $ noDataErr tagQ) (return . toAggR)
    (Just _) -> throwE "Illegal aggregation function"
    Nothing -> aggTag getList toCollect <&> toCollR <$> fromMaybe S.empty
