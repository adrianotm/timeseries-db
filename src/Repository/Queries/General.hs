{-# LANGUAGE RecordWildCards #-}
module Repository.Queries.General where

import           Control.Monad.Reader       (Reader, ask)
import           Control.Monad.Trans.Except
import           Data.Foldable

import           Aggregates
import           Repository.Model
import           Repository.Queries.Shared

aggGeneral :: Monoid m =>
    (m -> a)
    -> (TS -> m)
    -> ExceptQ (AggRes a m)
aggGeneral get to = ask >>= \InternalQ{qm=Q{..}, tdb=TimeseriesDB{..}}
                                 -> case groupBy of
                                         (Just GByTag) -> return $ toTagAggR $! foldMap' (\ts@TS{..} -> toGroup tag $ to ts) _data'
                                         (Just GByTimestemp) -> return $ toTSAggR $! foldMap' (\ts@TS{..} -> toGroup timestamp $ to ts) _data'
                                         Nothing -> return $ toCollAggR $ get $! foldMap' to _data'
                                         (Just IllegalGBy) -> throwE "Illegal 'groupBy' field."
