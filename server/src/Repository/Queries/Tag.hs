{-# LANGUAGE RecordWildCards #-}

module Repository.Queries.Tag (queryTag) where

-- import           Control.Monad.Par
import           Control.Monad.Reader        (Reader, ask)
import           Control.Monad.Trans.Except  (throwE)
import           Control.Parallel.Strategies
import           Data.Bifunctor              (second)
import           Data.Foldable               (Foldable (foldMap', foldl'))
import           Data.Functor                ((<&>))
import           Data.Traversable            (traverse)
import qualified Data.Vector                 as V
import qualified DataS.HashMap               as HM
import qualified DataS.IntMap                as IM
import           Repository.Model            (GroupBy (..), Ix, QueryModel (..),
                                              Tag, TimeseriesDB (..))
import           Repository.Queries.Utils    (AggRes, ExceptQ, InternalQ (..),
                                              noDataErr, qmToF, toCollAggR,
                                              toTSAggR, toTagAggR)

-- | Helper function when a 'tagEq' is present
--   do aggregations depending on the 'groupBy'
queryTag' :: Monoid m => Tag -> IM.IntMap Ix -> (m -> a) -> (Ix -> m) -> ExceptQ (AggRes a m)
queryTag' tag im get to =
  ask <&> \InternalQ {qm = qm@Q {..}, tdb = TimeseriesDB {..}} ->
    case groupBy of
      (Just GByTag) -> toTagAggR [(tag, foldMap' to $ qmToF qm im)]
      (Just GByTimestamp) -> toTSAggR $ IM.foldMapWithKey sort (\ts ix -> [(ts, to ix)]) (qmToF qm im)
      _ -> toCollAggR $ get $ IM.foldMap aggFunc sort to (qmToF qm im)

-- | Helper function when 'groupBy = "tag"' exists in the query, but not 'tagEq'
--   aggregate each group in parallel
--   if a 'tsEq' is present, filter out the tags with no data for that timestamp
groupTag :: (NFData m, Monoid m) => (Ix -> m) -> ExceptQ (AggRes a m)
groupTag to =
  ask >>= \InternalQ {qm = qm@Q {..}, tdb = TimeseriesDB {..}} ->
    case tsEq of
      Nothing ->
        return $
          toTagAggR
            ( map
                (second (foldMap' to . qmToF qm))
                ( HM.toList $
                    HM.filter (not . IM.null . qmToF qm) _sIx
                )
                `using` parBuffer 100 rdeepseq
            )
      Just ts ->
        return $
          toTagAggR $
            HM.foldMapWithKey (\tag ix -> [(tag, to ix)]) $
              HM.mapMaybe (IM.lookup ts) _sIx

-- | Query by the tag index
--   the query either has a 'groupBy = "tag"' or 'tagEq' or both
--   throw an error if no data exists for a tag
queryTag :: (NFData m, Monoid m) => (m -> a) -> (Ix -> m) -> ExceptQ (AggRes a m)
queryTag get to =
  ask >>= \InternalQ {qm = qm@Q {..}, tdb = TimeseriesDB {..}} ->
    case tagEq of
      Nothing -> groupTag to
      (Just tag) -> case HM.lookup tag _sIx of
        Nothing -> throwE $ noDataErr (Left tag)
        (Just im) -> case tsEq of
          Nothing -> queryTag' tag im get to
          (Just ts) -> maybe (throwE $ noDataErr (Right ts)) (return . toCollAggR . get . to) (IM.lookup ts im)
