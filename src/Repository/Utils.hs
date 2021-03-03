{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards  #-}
module Repository.Utils where

import           Control.Monad
import           Data.Conduit
import           Data.Conduit.Combinators  as CC
import           Data.Maybe                (mapMaybe)
import qualified Data.Vector               as V
import qualified Data.Vector.Mutable       as VM
import qualified DataS.HashMap             as HM
import qualified DataS.IntMap              as IM

import           Aggregates
import           Repository.Model
import           Repository.Queries
import           Repository.Queries.Shared

type Error = String

unsafeIndexOf :: TS -> TimeseriesDB -> Ix
unsafeIndexOf TS{..} TimeseriesDB{..} = (_tIx IM.! timestamp) HM.! tag

errMsgUpdate :: TS -> Error
errMsgUpdate TS{..} = "Timestamp = " ++ show timestamp ++ " and tag = " ++ show tag ++ " not found."

errMsgInsert :: TS -> Error
errMsgInsert TS{..} = "Timestamp = " ++ show timestamp ++ " and tag = " ++ show tag ++ " already exists."

validUpdate :: TimeseriesDB -> [TS] -> [Error]
validUpdate TimeseriesDB{..} = mapMaybe (\ts@TS{..} -> maybe (Just $ errMsgUpdate ts) (const Nothing) (HM.lookup tag =<< IM.lookup timestamp _tIx))

validInsert :: TimeseriesDB -> [TS] -> [Error]
validInsert TimeseriesDB{..} = mapMaybe (\ts@TS{..} -> const (Just $ errMsgInsert ts) =<< HM.lookup tag =<< IM.lookup timestamp _tIx)

illegalQM :: QueryModel -> (Bool, String)
illegalQM (Q Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just _)) = (True, "Only 'group' provided.")
illegalQM Q {gt = (Just _), ge = (Just _)}          = (True, "Can't query 'gt' and 'ge' at the same time.")
illegalQM Q {lt = (Just _), le = (Just _)}          = (True, "Can't query 'lt' and 'le' at the same time.")
illegalQM Q {tsEq = (Just _), gt = (Just _)}        = (True, "Can't query 'tsEq' with any other timestamp condition.")
illegalQM Q {tsEq = (Just _), ge = (Just _)}        = (True, "Can't query 'tsEq' with any other timestamp condition.")
illegalQM Q {tsEq = (Just _), lt = (Just _)}        = (True, "Can't query 'tsEq' with any other timestamp condition.")
illegalQM Q {tsEq = (Just _), le = (Just _)}        = (True, "Can't query 'tsEq' with any other timestamp condition.")
illegalQM Q {groupBy = (Just _), aggFunc = Nothing} = (True, "You must provie 'aggFunc' with 'group'.")
illegalQM _                                         = (False, "")

tIxAppendTS :: [TS] -> TimestampIndex -> Ix -> TimestampIndex
tIxAppendTS ts im ix = IM.unionWith HM.union im appendIM
  where appendIM = IM.fromList $ [(timestamp, HM.fromList [(tag, i)]) | TS{..} <- ts | i <- [ix..]]

sIxAppendTS :: [TS] -> TagIndex -> Ix -> TagIndex
sIxAppendTS ts m ix = HM.unionWith IM.union m appendIM
  where appendIM = HM.fromListWith IM.union appIM
        appIM = [(tag, IM.fromList [(timestamp, i)]) | TS{..} <- ts | i <- [ix..]]
