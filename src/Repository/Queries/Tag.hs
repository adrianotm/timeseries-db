{-# LANGUAGE RecordWildCards #-}
module Repository.Queries.Tag where

import           Control.Monad.Reader       (Reader, ask)
import           Control.Monad.Trans.Except
import           Data.Foldable
import           Data.Functor
import qualified Data.Vector                as V
import qualified DataS.DList                as DL
import qualified DataS.HashMap              as HM
import qualified DataS.IntMap               as IM

import           Aggregates
import           Repository.Model
import           Repository.Queries.Shared

queryTag' :: Monoid m => Tag -> IM.IntMap Ix -> (m -> a) -> (TS -> m) -> ExceptQ (AggRes a m)
queryTag' tag im get to = ask <&> \InternalQ{qm=qm@Q{..},tdb=TimeseriesDB{..}}
                                      -> case groupBy of
                                           (Just GByTag) -> toTagAggR $ toCollect (tag, foldMap' (to . getTS _data') (qmToF qm im))
                                           (Just GByTimestamp) -> toTSAggR $ IM.foldMapWithKey sort (\ts ix -> toCollect(ts, to $ getTS _data' ix)) (qmToF qm im)
                                           _ -> toCollAggR $ get $ IM.foldMap aggFunc sort (to . getTS _data') (qmToF qm im)

queryTag :: Monoid m => (m -> a) -> (TS -> m) -> ExceptQ (AggRes a m)
queryTag get to = ask >>= \InternalQ{qm=qm@Q{..},tdb=TimeseriesDB{..}}
                              -> case tagEq of
                                   Nothing -> return $ toTagAggR $ HM.foldMapWithKey (\tag im -> toCollect (tag, foldMap' (to . getTS _data') (qmToF qm im))) _sIx
                                   (Just tag) -> case HM.lookup tag _sIx of
                                       Nothing  -> throwE $ noDataErr (Left tag)
                                       (Just im) -> case tsEq of
                                              Nothing -> queryTag' tag im get to
                                              (Just ts) -> maybe (throwE $ noDataErr (Right ts)) (return . toCollAggR . get . to . getTS _data') (IM.lookup ts im)
