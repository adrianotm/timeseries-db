{-# LANGUAGE RecordWildCards #-}

module Repository.Queries.Utils where

import           Control.Monad.Except (ExceptT, MonadError (throwError))
import           Control.Monad.Reader (Reader)
import           Data.MonoTraversable (Element, MonoFoldable, ofoldl')
import qualified Data.Vector          as V
import qualified Data.Vector.Unboxed  as UV
import qualified DataS.IntMap         as IM
import           Repository.Model     (AggR (..), GroupAggR (..),
                                       GroupBy (GByTag), Ix, Limit,
                                       QueryModel (..), QueryR (..), TS (..),
                                       TS' (..), Tag, TimeseriesDB (..),
                                       Timestamp, Val)

simpleTS :: TS -> TS'
simpleTS (TS time tag val) = TS' time tag

-- | Filter the IntMap in a range, depending on the query parameters
qmToF :: QueryModel -> (IM.IntMap a -> IM.IntMap a)
qmToF Q {gt = (Just gt), lt = (Just lt)} = IM.getGT False gt . IM.getLT False lt
qmToF Q {gt = (Just gt), le = (Just le)} = IM.getGT False gt . IM.getLT True le
qmToF Q {lt = (Just lt), ge = (Just ge)} = IM.getGT True ge . IM.getLT False lt
qmToF Q {ge = (Just ge), le = (Just le)} = IM.getGT True ge . IM.getLT True le
qmToF Q {gt = (Just gt)} = IM.getGT False gt
qmToF Q {ge = (Just ge)} = IM.getGT True ge
qmToF Q {lt = (Just lt)} = IM.getLT False lt
qmToF Q {le = (Just le)} = IM.getLT True le
qmToF Q {} = id

data QueryType = TSQuery | TagQuery

data InternalQ = InternalQ
  { qm  :: QueryModel,
    tdb :: TimeseriesDB
  }

type ExceptQ = ExceptT String (Reader InternalQ)

-- | Type for the grouping aggregations
type GroupEither v = Either [(Tag, v)] [(Timestamp, v)]

-- | Type for the aggregation results
type AggRes a v = Either a (GroupEither v)

noDataErr :: Either Tag Timestamp -> String
noDataErr (Left tg)  = "No data for tag " ++ show tg ++ "."
noDataErr (Right ts) = "No data for timestamp " ++ show ts ++ "."

-- | Create a TS by combining the two vectors
makeTS :: TimeseriesDB -> Ix -> TS
makeTS TimeseriesDB {..} ix = TS timestamp' tag' val
  where
    TS' {..} = V.unsafeIndex _data' ix
    val = UV.unsafeIndex _dataV' ix

toQR :: Val -> QueryR
toQR = QR . Right . Right . AggR
{-# INLINE toQR #-}

toCollR :: [TS] -> QueryR
toCollR = QR . Left
{-# INLINE toCollR #-}

-- | Throw an error if the average failed
handleAvg :: Monad m => String -> Maybe Val -> ExceptT String m QueryR
handleAvg err = maybe (throwError err) (return . toQR)
{-# INLINE handleAvg #-}

toCollAggR :: a -> AggRes a v
toCollAggR = Left
{-# INLINE toCollAggR #-}

toTagAggR :: [(Tag, v)] -> AggRes a v
toTagAggR = Right . Left
{-# INLINE toTagAggR #-}

toTSAggR :: [(Timestamp, v)] -> AggRes a v
toTSAggR = Right . Right
{-# INLINE toTSAggR #-}

-- | Transform grouped aggregates to a QueryR
--   use the 'limit' parameter
toQRG :: Semigroup v => (v -> Val) -> Maybe Limit -> Either [(Tag, v)] [(Timestamp, v)] -> QueryR
toQRG f limit m = QR $ Right $ Left $ maybe id take limit $ either (trans Left) (trans Right) m
  where
    trans keyF l = map (\(k, v) -> GroupAggR (keyF k) (f v)) l

-- | Decide whether to query the timestamp index or the tag/timestamp index
--   if 'tagEq' or 'groupBy = "tag"' are present in the query, use the tag/timestamp index
--   otherwise use the timestamp index
qmToQT :: QueryModel -> QueryType
qmToQT Q {tagEq = (Just _)}        = TagQuery
qmToQT Q {groupBy = (Just GByTag)} = TagQuery
qmToQT _                           = TSQuery

-- | Helper function for a strict foldMap
ofoldMap' :: (MonoFoldable mono, Monoid m) => (Element mono -> m) -> mono -> m
ofoldMap' f = ofoldl' (\acc a -> acc <> f a) mempty
