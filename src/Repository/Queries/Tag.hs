{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE LambdaCase        #-}
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
import qualified Data.Vector          as V
import qualified Map                  as M

import           Aggregates
import           Repository.Model


aggTag :: Monoid m =>
    (m -> a)
    -> (TS -> m)
    -> Tag
    -> TimeseriesDB
    -> Maybe a
aggTag get to tg TimeseriesDB{..} = M.lookup tg sIx <&> get . foldMap' (to . (V.!) data')

tagQuery :: Maybe String -> Tag -> ExceptT String (Query TimeseriesDB) AggregateR
tagQuery (Just agg) tg | agg == "avg" = ask <&> join . aggTag getAverage (toAvg . value) tg <&> \case
                                                                                                    (Just a) -> Right $ AvgR a
                                                                                                    Nothing -> Right $ AvgR 0
                       | otherwise = throwError "Illegal aggregation"
tagQuery Nothing tg    = ask <&> aggTag getList toCollect tg <&> Left <$> fromMaybe []

