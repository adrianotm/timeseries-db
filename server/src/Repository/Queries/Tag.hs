{-# LANGUAGE RecordWildCards #-}
module Repository.Queries.Tag where

import           Control.Monad.Reader       (Reader, ask)
import           Control.Monad.Trans.Except (throwE)
import           Data.Foldable              (Foldable (foldMap'))
import           Data.Functor               ((<&>))
import qualified Data.Vector                as V
import qualified DataS.DList                as DL
import qualified DataS.HashMap              as HM
import qualified DataS.IntMap               as IM

import           Aggregates                 (toCollect)
import           Repository.Model           (GroupBy (..), Ix, QueryModel (..),
                                             Tag, TimeseriesDB (..))
import           Repository.Queries.Shared  (AggRes, ExceptQ, InternalQ (..),
                                             noDataErr, qmToF, toCollAggR,
                                             toTSAggR, toTagAggR)

queryTag' :: Monoid m => Tag -> IM.IntMap Ix -> (m -> a) -> (Ix -> m) -> ExceptQ (AggRes a m)
queryTag' tag im get to = ask <&> \InternalQ{qm=qm@Q{..},tdb=TimeseriesDB{..}}
                                      -> case groupBy of
                                           (Just GByTag) -> toTagAggR $ toCollect (tag, foldMap' to $ qmToF qm im)
                                           (Just GByTimestamp) -> toTSAggR $ IM.foldMapWithKey sort (\ts ix -> toCollect(ts, to ix)) (qmToF qm im)
                                           _ -> toCollAggR $ get $ IM.foldMap aggFunc sort to (qmToF qm im)

queryTag :: Monoid m => (m -> a) -> (Ix -> m) -> ExceptQ (AggRes a m)
queryTag get to = ask >>= \InternalQ{qm=qm@Q{..},tdb=TimeseriesDB{..}}
                              -> case tagEq of
                                   Nothing -> return $ toTagAggR
                                                     $ HM.foldMapWithKey (\tag im -> toCollect (tag, foldMap' to (qmToF qm im)))
                                                     $ HM.filter (not . IM.null . qmToF qm) _sIx
                                   (Just tag) -> case HM.lookup tag _sIx of
                                       Nothing  -> throwE $ noDataErr (Left tag)
                                       (Just im) -> case tsEq of
                                              Nothing -> queryTag' tag im get to
                                              (Just ts) -> maybe (throwE $ noDataErr (Right ts)) (return . toCollAggR . get . to) (IM.lookup ts im)
