{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards  #-}
module Repository.Utils where

import           Data.List           as L (foldl', map, reverse, sort, (\\))
import           Data.Maybe          (mapMaybe)
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as VM
import           DataS.DList         as DL (append, fromList, singleton, toList)
import qualified DataS.HashMap       as HM
import qualified DataS.IntMap        as IM

import           Repository.Model    (DTS (..), Ix, QueryModel (..), TS (..),
                                      TagIndex, TimeseriesDB (..),
                                      TimestampIndex)

type Error = String

unsafeIndexOf :: Either TS DTS -> TimeseriesDB -> Ix
unsafeIndexOf (Left TS{..}) TimeseriesDB{..} = (_sIx HM.! tag) IM.! timestamp
unsafeIndexOf (Right DTS{..}) TimeseriesDB{..} = (_sIx HM.! __tag) IM.! __timestamp

errMsgUpdate :: TS -> Error
errMsgUpdate TS{..} = "Timestamp = " ++ show timestamp ++ " and tag = " ++ show tag ++ " not found."

errMsgInsert :: TS -> Error
errMsgInsert TS{..} = "Timestamp = " ++ show timestamp ++ " and tag = " ++ show tag ++ " already exists."

errMsgDelete :: DTS -> Error
errMsgDelete DTS{..} = "Timestamp = " ++ show __timestamp ++ " and tag = " ++ show __tag ++ " not found."

validUpdate :: TimeseriesDB -> [TS] -> [Error]
validUpdate TimeseriesDB{..} = mapMaybe (\ts@TS{..} -> maybe (Just $ errMsgUpdate ts) (const Nothing) (IM.lookup timestamp =<< HM.lookup tag _sIx))

validDelete :: TimeseriesDB -> [DTS] -> [Error]
validDelete TimeseriesDB{..} = mapMaybe (\ts@DTS{..} -> maybe (Just $ errMsgDelete ts) (const Nothing) (IM.lookup __timestamp =<< HM.lookup __tag _sIx))

validInsert :: TimeseriesDB -> [TS] -> [Error]
validInsert TimeseriesDB{..} = mapMaybe (\ts@TS{..} -> const (Just $ errMsgInsert ts) =<< IM.lookup timestamp =<< HM.lookup tag _sIx)

illegalQM :: QueryModel -> (Bool, String)
illegalQM Q {groupBy = (Just _), aggFunc = Nothing} = (True, "You must provie 'aggFunc' with 'groupBy'.")
illegalQM Q {gt = (Just _), ge = (Just _)}          = (True, "Can't query 'gt' and 'ge' at the same time.")
illegalQM Q {lt = (Just _), le = (Just _)}          = (True, "Can't query 'lt' and 'le' at the same time.")
illegalQM Q {tsEq = (Just _), gt = (Just _)}        = (True, "Can't query 'tsEq' with any other timestamp condition.")
illegalQM Q {tsEq = (Just _), ge = (Just _)}        = (True, "Can't query 'tsEq' with any other timestamp condition.")
illegalQM Q {tsEq = (Just _), lt = (Just _)}        = (True, "Can't query 'tsEq' with any other timestamp condition.")
illegalQM Q {tsEq = (Just _), le = (Just _)}        = (True, "Can't query 'tsEq' with any other timestamp condition.")
illegalQM Q {gt = (Just _), lt = (Just _)}          = (True, "Can't query 'tsEq' with any other timestamp condition.")
illegalQM _                                         = (False, "")

tIxAppendTS :: [TS] -> TimestampIndex -> Ix -> TimestampIndex
tIxAppendTS ts im ix = IM.unionWith DL.append im appendIM
  where appendIM = IM.fromListWith DL.append $ [(timestamp, DL.singleton i) | TS{..} <- ts | i <- [ix..]]

sIxAppendTS :: [TS] -> TagIndex -> Ix -> TagIndex
sIxAppendTS ts m ix = HM.unionWith IM.union m appendIM
  where appendIM = HM.fromListWith IM.union appIM
        appIM = [(tag, IM.fromList [(timestamp, i)]) | TS{..} <- ts | i <- [ix..]]

tIxDeleteTS :: [DTS] -> TimeseriesDB -> TimestampIndex
tIxDeleteTS dtss db@TimeseriesDB{..} = IM.differenceWith f _tIx dim
  where dim = IM.fromListWith DL.append [(__timestamp, DL.singleton $ unsafeIndexOf (Right dts) db) | dts@DTS{..} <- dtss ]
        f dl1 dl2 = case DL.toList dl1 \\ DL.toList dl2 of
                      []  -> Nothing
                      ixs -> Just $ DL.fromList ixs

sIxDeleteTS :: [DTS] -> TimeseriesDB -> TagIndex
sIxDeleteTS dtss db@TimeseriesDB{..} = HM.differenceWith f _sIx dhm
  where dhm = HM.fromListWith IM.union
                  [(__tag, IM.singleton __timestamp $ unsafeIndexOf (Right dts) db) | dts@DTS{..} <- dtss]
        f im1 im2 = let im = IM.difference im1 im2 in
                        if im == IM.empty then Nothing else Just im

vDeleteTS :: [DTS] -> TimeseriesDB -> V.Vector TS
vDeleteTS dtss db@TimeseriesDB{..} = V.concat $ L.reverse $ foldl' f [] $ L.sort $ L.map (flip unsafeIndexOf db . Right) dtss
  where f [] ix      = let (spl1, spl2) = V.splitAt ix _data' in [spl2, spl1]
        f (v:acc) ix = let (spl1, spl2) = V.splitAt ix v in spl2 : spl1 : acc
