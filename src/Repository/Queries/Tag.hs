{-# LANGUAGE RecordWildCards #-}
module Repository.Queries.Tag where

import           Control.Monad.Reader       (Reader, ask)
import           Control.Monad.Trans.Except
import           Data.Foldable
import qualified Data.Vector                as V
import qualified DataS.Map                  as M

import           Aggregates
import           Repository.Model
import           Repository.Queries.Shared

noDataErr :: Tag -> String
noDataErr tg = "No data for tag " ++ either show show tg

aggTag :: Monoid m =>
    Tag
    -> (m -> a)
    -> (TS -> m)
    -> ExceptQ (AggRes a m)
aggTag tag get to = ask >>= \InternalQ{qm=Q{..}, ..}
                              -> case M.lookup tag (sIx tdb) of
                                 Nothing  -> throwE $ noDataErr tag
                                 (Just dl) -> case groupBy of
                                               (Just GByTag) -> return $! toTagAggR $ foldMap' (toGroup tag . to . (V.!) (data' tdb)) dl
                                               Nothing -> return $! toCollAggR $ get $ foldMap' (to . (V.!) (data' tdb)) dl
                                               (Just GByTimestemp) -> throwE "Can't use 'groupBy = timestamp with 'tagEq'."
                                               (Just IllegalGBy) -> throwE "Illegal 'groupBy' field."
