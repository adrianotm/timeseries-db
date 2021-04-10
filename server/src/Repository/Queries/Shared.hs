{-# LANGUAGE RecordWildCards #-}
module Repository.Queries.Shared where

import           Control.Monad.Except (ExceptT)
import           Control.Monad.Reader (Reader)

import qualified Data.Vector          as V
import qualified Data.Vector.Unboxed  as UV
import qualified DataS.IntMap         as IM
import           Repository.Model     (GroupAggR (..), GroupBy (GByTag), Ix,
                                       Limit, QueryModel (..), QueryR (..),
                                       TS (..), TS' (..), Tag,
                                       TimeseriesDB (..), Timestamp, Val)

simpleTS :: TS -> TS'
simpleTS (TS time tag val) = TS' time tag

qmToF :: QueryModel -> (IM.IntMap a -> IM.IntMap a)
qmToF Q {gt = (Just gt), lt = (Just lt)} = IM.getGT False gt . IM.getLT False lt
qmToF Q {gt = (Just gt), le = (Just le)} = IM.getGT False gt . IM.getLT True le
qmToF Q {lt = (Just lt), ge = (Just ge)} = IM.getGT True ge . IM.getLT False lt
qmToF Q {ge = (Just ge), le = (Just le)} = IM.getGT True ge . IM.getLT True le
qmToF Q {gt = (Just gt)}                 = IM.getGT False gt
qmToF Q {ge = (Just ge)}                 = IM.getGT True ge
qmToF Q {lt = (Just lt)}                 = IM.getLT False lt
qmToF Q {le = (Just le)}                 = IM.getLT True le
qmToF Q {}                               = id

data QueryType = TSQuery | TagQuery

data InternalQ = InternalQ { qm  :: QueryModel
                           , tdb :: TimeseriesDB
                           }

type ExceptQ = ExceptT String (Reader InternalQ)

type GroupEither v = Either [(Tag, v)] [(Timestamp, v)]

type AggRes a v = Either a (GroupEither v)

noDataErr :: Either Tag Timestamp -> String
noDataErr (Left tg)  = "No data for tag " ++ show tg ++ "."
noDataErr (Right ts) = "No data for timestamp " ++ show ts ++ "."

composeTS :: TimeseriesDB -> Ix -> TS
composeTS TimeseriesDB{..} ix = TS timestamp' tag' val
    where TS'{..} = V.unsafeIndex _data' ix
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

toQRG :: Semigroup v => (v -> Val) -> Maybe Limit -> Either [(Tag, v)] [(Timestamp, v)] -> QueryR
toQRG f limit m = QR $ Right $ Left $ maybe id take limit $ either (trans Left) (trans Right) m
    where trans keyF l = map (\(k,v) -> GroupAggR (keyF k) (f v)) l

qmToQT :: QueryModel -> QueryType
qmToQT Q {tagEq = (Just _)}        = TagQuery
qmToQT Q {groupBy = (Just GByTag)} = TagQuery
qmToQT _                           = TSQuery
