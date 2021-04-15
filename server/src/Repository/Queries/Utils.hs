{-# LANGUAGE RecordWildCards #-}

module Repository.Queries.Utils where

import           Control.Monad.Except (ExceptT)
import           Control.Monad.Reader (Reader)
import           Data.MonoTraversable (Element, MonoFoldable, ofoldl')
import qualified Data.Vector          as V
import qualified Data.Vector.Unboxed  as UV
import qualified DataS.IntMap         as IM
import           Repository.Model     (GroupAggR (..), GroupBy (GByTag), Ix,
                                       Limit, QueryModel (..), QueryR (..),
                                       TS (..), TS' (..), Tag,
                                       TimeseriesDB (..), Timestamp, Val)

simpleTS :: TS -> TS'
simpleTS (TS time tag val) = TS' time tag

-- Filter the Timestamp index depending on the query
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

type GroupEither v = Either [(Tag, v)] [(Timestamp, v)]

type AggRes a v = Either a (GroupEither v)

noDataErr :: Either Tag Timestamp -> String
noDataErr (Left tg)  = "No data for tag " ++ show tg ++ "."
noDataErr (Right ts) = "No data for timestamp " ++ show ts ++ "."

-- Create a TS by combining the two vectors
makeTS :: TimeseriesDB -> Ix -> TS
makeTS TimeseriesDB {..} ix = TS timestamp' tag' val
  where
    TS' {..} = V.unsafeIndex _data' ix
    val = UV.unsafeIndex _dataV' ix

toCollAggR :: a -> AggRes a v
toCollAggR = Left
{-# INLINE toCollAggR #-}

toTagAggR :: [(Tag, v)] -> AggRes a v
toTagAggR = Right . Left
{-# INLINE toTagAggR #-}

toTSAggR :: [(Timestamp, v)] -> AggRes a v
toTSAggR = Right . Right
{-# INLINE toTSAggR #-}

-- Transform grouped aggregates to QueryR
toQRG :: Semigroup v => (v -> Val) -> Maybe Limit -> Either [(Tag, v)] [(Timestamp, v)] -> QueryR
toQRG f limit m = QR $ Right $ Left $ maybe id take limit $ either (trans Left) (trans Right) m
  where
    trans keyF l = map (\(k, v) -> GroupAggR (keyF k) (f v)) l

-- Decide whether to use the Tag index or the Timestamp index
qmToQT :: QueryModel -> QueryType
qmToQT Q {tagEq = (Just _)}        = TagQuery
qmToQT Q {groupBy = (Just GByTag)} = TagQuery
qmToQT _                           = TSQuery

ofoldMap' :: (MonoFoldable mono, Monoid m) => (Element mono -> m) -> mono -> m
ofoldMap' f = ofoldl' (\acc a -> acc <> f a) mempty
