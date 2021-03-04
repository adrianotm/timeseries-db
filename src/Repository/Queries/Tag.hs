{-# LANGUAGE RecordWildCards #-}
module Repository.Queries.Tag where

import           Control.Monad.Reader       (Reader, ask)
import           Control.Monad.Trans.Except
import           Data.Foldable
import qualified Data.Vector                as V
import qualified DataS.HashMap              as HM
import qualified DataS.IntMap               as IM

import           Aggregates
import           Repository.Model
import           Repository.Queries.Shared

noDataErr :: Either Tag Timestamp -> String
noDataErr (Left tg)  = "No data for tag " ++ show tg
noDataErr (Right ts) = "No data for timestamp " ++ show ts

queryTag' :: Monoid m => Tag -> IM.IntMap Ix -> (m -> a) -> (TS -> m) -> ExceptQ (AggRes a m)
queryTag' tag im get to = ask >>= \InternalQ{qm=qm@Q{..},tdb=TimeseriesDB{..}}
                                     -> case groupBy of
                                          (Just GByTag)       -> return $ toTagAggR $! foldMap' (toGroup tag . to . (V.!) _data') (qmToF qm im)
                                          (Just GByTimestemp) -> return $ toTSAggR $! IM.foldMapWithKey' sort (\k -> toGroup k . to . (V.!) _data') (qmToF qm im)
                                          Nothing             -> return $ toCollAggR $ get $! IM.foldMap' sort (to . (V.!) _data') (qmToF qm im)

queryTag :: Monoid m => Tag -> (m -> a) -> (TS -> m) -> ExceptQ (AggRes a m)
queryTag tag get to = ask >>= \InternalQ{qm=Q{..},tdb=TimeseriesDB{..}}
                              -> case HM.lookup tag _sIx of
                                 Nothing  -> throwE $ noDataErr (Left tag)
                                 (Just im)
                                    -> case tsEq of
                                        Nothing -> queryTag' tag im get to
                                        (Just ts)
                                            -> maybe (throwE $ noDataErr (Right ts)) (return . toCollAggR . get . to . (V.!) _data') (IM.lookup ts im)
