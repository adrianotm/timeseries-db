{-# LANGUAGE RecordWildCards #-}
module Repository.Utils where

import           Control.Monad
import           Data.Maybe                (mapMaybe)
import qualified Data.Vector               as V
import qualified Data.Vector.Mutable       as VM
import qualified DataS.IntMap              as IM
import qualified DataS.Map                 as M

import           Aggregates
import           Repository.Model
import           Repository.Queries
import           Repository.Queries.Shared

type Error = String

unsafeIndexOf :: TS -> TimeseriesDB -> Ix
unsafeIndexOf TS{..} TimeseriesDB{..} = (_tIx IM.! timestamp) M.! tag

errMsgUpdate :: TS -> Error
errMsgUpdate TS{..} = "Timestamp = " ++ show timestamp ++ " and tag = " ++ either show show tag ++ " not found."

errMsgInsert :: TS -> Error
errMsgInsert TS{..} = "Timestamp = " ++ show timestamp ++ " and tag = " ++ either show show tag ++ " already exists."

validUpdate :: TimeseriesDB -> [TS] -> [Error]
validUpdate TimeseriesDB{..} = mapMaybe (\ts@TS{..} -> maybe (Just $ errMsgUpdate ts) (const Nothing) (M.lookup tag =<< IM.lookup timestamp _tIx))

validInsert :: TimeseriesDB -> [TS] -> [Error]
validInsert TimeseriesDB{..} = mapMaybe (\ts@TS{..} -> const (Just $ errMsgInsert ts) =<< M.lookup tag =<< IM.lookup timestamp _tIx)

illegalQM :: QueryModel -> (Bool, String)
illegalQM (Q Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just _)) = (True, "Only 'group' provided.")
illegalQM Q {gt = (Just _), ge = (Just _)}    = (True, "Can't query 'gt' and 'ge' at the same time.")
illegalQM Q {lt = (Just _), le = (Just _)}    = (True, "Can't query 'lt' and 'le' at the same time.")
illegalQM Q {tsEq = (Just _), gt = (Just _)}  = (True, "Can't query 'tsEq' with any other timestamp condition.")
illegalQM Q {tsEq = (Just _), ge = (Just _)}  = (True, "Can't query 'tsEq' with any other timestamp condition.")
illegalQM Q {tsEq = (Just _), lt = (Just _)}  = (True, "Can't query 'tsEq' with any other timestamp condition.")
illegalQM Q {tsEq = (Just _), le = (Just _)}  = (True, "Can't query 'tsEq' with any other timestamp condition.")
illegalQM Q {groupBy = (Just _), aggFunc = Nothing} = (True, "You must provie 'aggFunc' with 'group'.")
illegalQM _                                   = (False, "")
