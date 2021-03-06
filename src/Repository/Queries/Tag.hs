{-# LANGUAGE RecordWildCards #-}
module Repository.Queries.Tag where

import           Control.Monad.Reader       (Reader, ask)
import           Control.Monad.Trans.Except
import qualified Data.DList                 as DL
import           Data.Foldable
import           Data.Functor
import qualified Data.Vector                as V
import qualified DataS.HashMap              as HM
import qualified DataS.IntMap               as IM

import           Aggregates
import           Repository.Model
import           Repository.Queries.Shared

queryTag'' :: Monoid m => Tag -> IM.IntMap Ix -> (m -> a) -> (TS -> m) -> ExceptQ (AggRes a m)
queryTag'' tag im get to = ask <&> \InternalQ{qm=qm@Q{..},tdb=TimeseriesDB{..}}
                                      -> case groupBy of
                                           (Just GByTag) -> toTagAggR $ toCollect (tag, foldMap' (to . getTS _data') (qmToF qm im))
                                           (Just GByTimestamp) -> toTSAggR $ IM.foldMapWithKey' sort (\k v -> toCollect(k, to $ getTS _data' v)) (qmToF qm im)
                                           _ -> toCollAggR $ get $ IM.foldMap' sort (to . getTS _data') (qmToF qm im)

queryTag :: Monoid m => (m -> a) -> (TS -> m) -> ExceptQ (AggRes a m)
queryTag get to = ask >>= \InternalQ{qm=qm@Q{..},tdb=TimeseriesDB{..}}
                              -> case tagEq of
                                   Nothing -> return $ toTagAggR $ HM.foldMapWithKey' (\k v -> toCollect (k, foldMap' (to . getTS _data') (qmToF qm v))) _sIx
                                   (Just tag) -> case HM.lookup tag _sIx of
                                       Nothing  -> throwE $ noDataErr (Left tag)
                                       (Just im) -> case tsEq of
                                              Nothing -> queryTag'' tag im get to
                                              (Just ts) -> maybe (throwE $ noDataErr (Right ts)) (return . toCollAggR . get . to . getTS _data') (IM.lookup ts im)
