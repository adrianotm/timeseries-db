{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE BlockArguments    #-}
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
import           Data.Either
import           Data.Foldable
import           Data.Functor
import           Data.Maybe
import           Data.Monoid
import           Data.Semigroup
import qualified Data.Vector                as V
import qualified DataS.Map                  as M

import           Aggregates                 as A
import           Repository.Model

data TagQuery = TagQ { aggQ   :: Maybe Agg
                     , tdb    :: TimeseriesDB
                     , groupQ :: Maybe GroupBy
                     , tagQ   :: Tag
                     }

type AggRes a v = Either a (M.Map Tag v)

type ExceptTagQuery = ExceptT String (Reader TagQuery)

toCollAggR :: a -> AggRes a v
toCollAggR = Left

toTagAggR :: Group Tag v -> AggRes a v
toTagAggR = Right . getGroup

noDataErr :: Tag -> String
noDataErr tg = "No data for tag " ++ either show show tg

aggTag :: Monoid m =>
    (m -> a)
    -> (TS -> m)
    -> ExceptTagQuery (AggRes a m)
aggTag get to = ask >>= \TagQ{..}
                            -> case M.lookup tagQ (sIx tdb) of
                                 Nothing  -> throwE $ noDataErr tagQ
                                 (Just dl) -> case groupQ of
                                               (Just GByTag) -> return $! toTagAggR $ foldMap' (toGroup tagQ . to . (V.!) (data' tdb)) dl
                                               Nothing -> return $! toCollAggR $ get $ foldMap' (to . (V.!) (data' tdb)) dl
                                               (Just GByTimestemp) -> throwE "Can't use 'groupBy = timestamp with 'tagEq'."
                                               (Just IllegalGBy) -> throwE "Illegal 'groupBy' field."

tagQuery :: ExceptTagQuery QueryR
tagQuery = ask
            >>= \TagQ{..}
              -> case aggQ of
    (Just AvgAgg) ->  aggTag getAverage (toAvg . value) >>=
                                either (handleAgg "Average failed")
                                       (return . toAggRG (fromMaybe 0 . getAverage) . Left)
    (Just SumAgg) ->  aggTag getSum (Sum . value) <&> either toAggR (toAggRG getSum . Left)
    (Just CountAgg) ->  aggTag getSum (const $ Sum 1) <&> either toAggR (toAggRG getSum . Left)
    (Just MinAgg) ->  aggTag getMin (Min . value) <&> either toAggR (toAggRG getMin . Left)
    (Just MaxAgg) ->  aggTag getMax (Max . value) <&> either toAggR (toAggRG getMax . Left)
    (Just IllegalAgg) -> throwE "Illegal aggregation function"
    Nothing -> aggTag getList toCollect <&> toCollR . fromLeft DL.empty
