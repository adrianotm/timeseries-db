module Repository.Queries.Shared where

import           Control.Monad.Except (ExceptT)
import           Control.Monad.Reader (Reader)

import           Aggregates
import qualified Data.DList           as DL
import           Data.Foldable
import qualified Data.Map.Strict      as M
import qualified Data.Vector          as V
import qualified DataS.IntMap         as IM
import           Repository.Model

qmToF :: QueryModel -> (IM.IntMap a -> IM.IntMap a)
qmToF Q {gt = (Just gt), lt = (Just lt)} = IM.lookupGLT' False False gt lt
qmToF Q {gt = (Just gt), le = (Just le)} = IM.lookupGLT' False True gt le
qmToF Q {lt = (Just lt), ge = (Just ge)} = IM.lookupGLT' True False ge lt
qmToF Q {ge = (Just ge), le = (Just le)} = IM.lookupGLT' True True ge le
qmToF Q {gt = (Just gt)}                 = IM.lookupGT' False gt
qmToF Q {ge = (Just ge)}                 = IM.lookupGT' True ge
qmToF Q {lt = (Just lt)}                 = IM.lookupLT' False lt
qmToF Q {le = (Just le)}                 = IM.lookupLT' True le
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

takeDL :: Int -> DL.DList a -> DL.DList a
takeDL n = DL.fromList . take n . DL.toList

toAggRG :: Semigroup v => (v -> Val) -> Maybe Limit -> Either (DL.DList (Tag, v)) (DL.DList (Timestamp, v)) -> QueryR
toAggRG f limit m = QR $ Right $ Left $ either (trans Left) (trans Right) m
    where trans keyF dl = DL.toList $ foldl' (uncurry . conc' keyF) DL.empty (maybe id takeDL limit dl)
          conc' keyF acc k v = DL.snoc acc (GroupAggR (keyF k) (f v))

qmToQT :: QueryModel -> QueryType
qmToQT Q {tagEq = (Just _)}        = TagQuery
qmToQT Q {groupBy = (Just GByTag)} = TagQuery
qmToQT _                           = TSQuery
