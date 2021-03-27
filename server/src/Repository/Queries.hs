{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards  #-}
module Repository.Queries where


import           Aggregates                (getAverage, getCollList, handleAgg,
                                            toAvg, toCollR, toCollect, toQR)
import           Control.Lens              ((%~), (.~))
import           Control.Monad.Reader      (ask)
import           Data.Either               (fromLeft)
import           Data.Foldable             (forM_)
import           Data.Function             ((&))
import           Data.Functor              ((<&>))
import           Data.List                 as L (foldl', map, reverse, sort,
                                                 (\\))
import           Data.Maybe                (fromMaybe, mapMaybe)
import           Data.Monoid               (Sum (Sum, getSum))
import           Data.Semigroup            (Max (Max, getMax),
                                            Min (Min, getMin))
import qualified Data.Vector               as V
import qualified Data.Vector.Mutable       as VM
import qualified DataS.HashMap             as HM
import qualified DataS.IntMap              as IM
import           Repository.Queries.Shared (AggRes, ExceptQ,
                                            InternalQ (InternalQ, qm, tdb),
                                            QueryType (TSQuery, TagQuery),
                                            getTS, qmToQT, toQRG)
import           Repository.Queries.Tag    (queryTag)
import           Repository.Queries.TS     (queryTS)

import           Repository.Model          (Agg (..), DTS (..), Ix,
                                            QueryModel (..), QueryR (..),
                                            TS (..), TagIndex,
                                            TimeseriesDB (..), TimestampIndex,
                                            data')

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

tIxAppendTS :: [TS] -> TimestampIndex -> Ix -> TimestampIndex
tIxAppendTS ts im ix = foldl' (\acc (!ts, !inx) -> IM.insertWith (++) ts inx acc) im appIM
        where appIM = [(timestamp, [i]) | TS{..} <- ts | i <- [ix..]]

sIxAppendTS :: [TS] -> TagIndex -> Ix -> TagIndex
sIxAppendTS ts m ix = foldl' (\acc (!tag, !tix) -> HM.insertWith IM.union tag tix acc) m appIM
        where appIM = [(tag, IM.fromList [(timestamp, i)]) | TS{..} <- ts | i <- [ix..]]

tIxDeleteTS :: [DTS] -> TimeseriesDB -> TimestampIndex
tIxDeleteTS dtss db@TimeseriesDB{..} = IM.differenceWith f _tIx dim
  where dim = IM.fromListWith (++) [(__timestamp, [unsafeIndexOf (Right dts) db]) | dts@DTS{..} <- dtss ]
        f dl1 dl2 = case dl1 \\ dl2 of
                      []  -> Nothing
                      ixs -> Just ixs

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

vUpdateTS :: [TS] -> TimeseriesDB -> TimeseriesDB
vUpdateTS ts db = db & data' %~ V.modify (\v -> forM_ ts (\ts -> VM.write v (unsafeIndexOf (Left ts) db) ts))

queryF :: Monoid m => QueryModel -> (m -> a) -> (Ix -> m) -> ExceptQ (AggRes a m)
queryF qm = case qmToQT qm of
                TSQuery  -> queryTS
                TagQuery -> queryTag

query :: ExceptQ QueryR
query = ask
    >>= \InternalQ{qm=qm@Q{..},tdb=TimeseriesDB{..}}
        -> let toM to = to . value . getTS _data' in
               case aggFunc of
                    (Just AvgAgg) -> queryF qm getAverage (toM toAvg) >>=
                                                either (handleAgg "Average failed.")
                                                       (return . toQRG (fromMaybe 0 . getAverage) limit)
                    (Just SumAgg) ->  queryF qm getSum (toM Sum) <&> either toQR (toQRG getSum limit)
                    (Just CountAgg) ->  queryF qm getSum (const $ Sum 1) <&> either toQR (toQRG getSum limit)
                    (Just MinAgg) ->  queryF qm getMin (toM Min) <&> either toQR (toQRG getMin limit)
                    (Just MaxAgg) ->  queryF qm getMax (toM Max) <&> either toQR (toQRG getMax limit)
                    Nothing -> queryF qm getCollList (toCollect . getTS _data') <&> toCollR . maybe id take limit . fromLeft []
