{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
module Repository.Queries.Tag where

import           Control.Monad        (join)
import           Control.Monad.Except
import           Control.Monad.Reader (ask)
import           Data.Acid            (Query, Update, makeAcidic)
import           Data.Foldable
import           Data.Functor
import           Data.Maybe
import           Data.Monoid
import qualified Data.Vector          as V
import qualified DataS.Map            as M

import           Aggregates           as A
import           Repository.Model


aggTag :: Monoid m =>
    (m -> a)
    -> (TS -> m)
    -> Tag
    -> TimeseriesDB
    -> Maybe a
aggTag get to tg TimeseriesDB{..} = M.lookup tg sIx <&> get . foldMap' (to . (V.!) data')

tagQuery :: Maybe String
         -> Tag
         -> ExceptionQuery QueryR
tagQuery (Just agg) tg
  | agg == "avg" = ask >>= maybe (throwError "Average failed") (return . toAggR) . join . aggTag getAverage (toAvg . value) tg
  | agg == "sum" = ask >>= maybe (throwError $ "No data for tag " ++ either show show tg) (return . toAggR) . aggTag getSum (Sum . value) tg
  | agg == "count" = ask >>= maybe (return $ toAggR 0) (return . toAggR) . aggTag getSum (const $ Sum 1) tg
  | otherwise = throwError "Illegal aggregation function"
tagQuery Nothing tg    = ask <&> aggTag getList toCollect tg <&> toCollR <$> fromMaybe []
