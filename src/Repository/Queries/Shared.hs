module Repository.Queries.Shared where

import           Control.Monad.Except (ExceptT)
import           Control.Monad.Reader (Reader)

import           Aggregates
import qualified Data.Map.Strict      as M
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

data QueryType = TSQuery | TagQuery Tag

data InternalQ = InternalQ { qm  :: QueryModel
                           , tdb :: TimeseriesDB
                           }

type ExceptQ = ExceptT String (Reader InternalQ)

type GroupEither v = Either (M.Map Tag v) (M.Map Timestamp v)

type AggRes a v = Either a (GroupEither v)

toCollAggR :: a -> AggRes a v
toCollAggR = Left

toTagAggR :: Group Tag v -> AggRes a v
toTagAggR = Right . Left . getGroup

toTSAggR :: Group Timestamp v -> AggRes a v
toTSAggR = Right . Right . getGroup

toAggRG :: Semigroup v => (v -> Val) -> Either (M.Map Tag v) (M.Map Timestamp v) -> QueryR
toAggRG f m = QR $ Right $ Left $ either (trans Left) (trans Right) m
    where trans keyF = M.foldrWithKey' (\k v -> (:) (GroupAggR (keyF k) $ f v)) []

qmToQT :: QueryModel -> QueryType
qmToQT Q {tagEq = (Just t)} = TagQuery t
qmToQT _                    = TSQuery
