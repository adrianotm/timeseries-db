module Repository.Queries.Shared where

import           Control.Monad.Except (ExceptT)
import           Control.Monad.Reader (Reader)

import           Aggregates
import           Data.Foldable
import qualified Data.Map.Strict      as M
import qualified Data.Vector          as V
import qualified DataS.DList          as DL
import qualified DataS.IntMap         as IM
import           Model
import           Repository.Model

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

type GroupEither v = Either (DL.DList (Tag, v)) (DL.DList (Timestamp, v))

type AggRes a v = Either a (GroupEither v)

noDataErr :: Either Tag Timestamp -> String
noDataErr (Left tg)  = "No data for tag " ++ show tg
noDataErr (Right ts) = "No data for timestamp " ++ show ts

getTS :: V.Vector TS -> Ix -> TS
getTS = (V.!)

toCollAggR :: a -> AggRes a v
toCollAggR = Left

toTagAggR :: Collect (Tag, v) -> AggRes a v
toTagAggR = Right . Left . getList

toTSAggR :: Collect (Timestamp, v) -> AggRes a v
toTSAggR = Right . Right . getList

toQRG :: Semigroup v => (v -> Val) -> Maybe Limit -> Either (DL.DList (Tag, v)) (DL.DList (Timestamp, v)) -> QueryR
toQRG f limit m = QR $ Right $ Left $ maybe id take limit $ either (trans Left) (trans Right) m
    where trans keyF dl = foldr (\(k,v) acc -> GroupAggR (keyF k) (f v) : acc) [] dl

qmToQT :: QueryModel -> QueryType
qmToQT Q {tagEq = (Just _)}        = TagQuery
qmToQT Q {groupBy = (Just GByTag)} = TagQuery
qmToQT _                           = TSQuery
