{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
module Repository.Queries.TS where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Reader (ask)
import           Control.Monad.State  (MonadState, evalState, get, put)
import           Data.Acid            (Query, Update, makeAcidic)
import           Data.Foldable
import           Data.Functor
import qualified Data.Sequence        as S
import qualified Data.Vector          as V
import qualified IntMap               as IM
import qualified Map                  as M

import           Aggregates
import           Repository.Model

aggTS :: Monoid m =>
        (m -> a)
        -> (TS -> m)
        -> Maybe Tag
        -> (IM.IntMap TagMap -> IM.IntMap TagMap)
        -> TimeseriesDB
        -> a
aggTS get to tg f TimeseriesDB{..} = get $ foldMap' (mapToM to tg data') (f tIx)

mapToM :: Monoid m => (TS -> m) -> Maybe Tag -> V.Vector TS -> M.Map Tag Ix -> m
mapToM toM Nothing d m   = foldMap' (toM . (V.!) d) m
mapToM toM (Just tg) d m = maybe mempty (toM . (V.!) d) (M.lookup tg m)

tsQuery :: Maybe String
        -> Maybe Tag
        -> (IM.IntMap TagMap -> IM.IntMap TagMap)
        -> ExceptT String (Query TimeseriesDB) AggregateR
tsQuery (Just agg) tg f | agg == "avg" = ask <&> aggTS getAverage (toAvg . value) tg f <&> \case
                                                                                             (Just a) -> Right $ AvgR a
                                                                                             Nothing -> Right $ AvgR 0 --- CHANGE THIS
                        | otherwise = throwError "Illegal aggregation function"
tsQuery Nothing tg f    = ask <&> aggTS getList toCollect tg f <&> Left
