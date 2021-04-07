{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}
module Repository.Queries
  (Error,
  unsafeIndexOf,
  validUpdate,
  validInsert,
  validDelete,
  tIxAppendTS,
  sIxAppendTS,
  tIxDeleteTS,
  sIxDeleteTS,
  vDeleteTS,
  vUpdateTS,
  query)
  where


import           Aggregates                (Average, getAverage, handleAgg,
                                            toAvg, toCollR, toQR)
import           Control.DeepSeq           (force)
import           Control.Lens              ((%~), (.~))
import           Control.Monad.Reader      (ask)
import           Data.Either               (fromLeft)
import           Data.Foldable             (foldMap', forM_)
import           Data.Function             ((&))
import           Data.Functor              ((<&>))
import           Data.List                 as L (delete, foldl', map, reverse,
                                                 sort, (\\))
import           Data.Maybe                (fromMaybe, mapMaybe)
import           Data.Monoid               (Sum (Sum, getSum))
import           Data.Semigroup            (Max (Max, getMax),
                                            Min (Min, getMin))
import qualified Data.Vector               as V
import qualified Data.Vector.Mutable       as VM
import qualified DataS.HashMap             as HM
import qualified DataS.IntMap              as IM
import           Repository.Model          (Agg (..), DTS (..), Ix,
                                            QueryModel (..), QueryR (..),
                                            TS (..), TagIndex,
                                            TimeseriesDB (..), TimestampIndex,
                                            Val, data', onlyAgg)
import           Repository.Queries.Shared (AggRes, ExceptQ,
                                            InternalQ (InternalQ, qm, tdb),
                                            QueryType (TSQuery, TagQuery),
                                            getTS, qmToQT, toQRG)
import           Repository.Queries.Tag    (queryTag)
import           Repository.Queries.TS     (queryTS)

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
tIxAppendTS ts im ix =
  foldl' (\acc (ts, inx) -> IM.insertWith (++) ts inx acc) im appIM
        where appIM = [(timestamp, [i]) | TS{..} <- ts | i <- [ix..]]

sIxAppendTS :: [TS] -> TagIndex -> Ix -> TagIndex
sIxAppendTS ts m ix =
  foldl' f m appIM
        where appIM = [(tag, timestamp, i) | TS{..} <- ts | i <- [ix..]]
              f acc (tag, timestamp, ix)
                = case HM.lookup tag acc of
                     Nothing -> HM.insert tag (IM.fromList [(timestamp, ix)]) acc
                     (Just im) -> HM.insert tag (IM.insert timestamp ix im) acc

tIxDeleteTS :: [DTS] -> TimeseriesDB -> TimestampIndex
tIxDeleteTS dtss db@TimeseriesDB{..} =
  foldl' (\acc (ts, ix) -> IM.update (f ix) ts acc) _tIx dts
    where dts = [(__timestamp, unsafeIndexOf (Right dts) db) | dts@DTS{..} <- dtss ]
          f ix l = case L.delete ix l of
                          []  -> Nothing
                          ixs -> Just ixs

sIxDeleteTS :: [DTS] -> TimeseriesDB -> TagIndex
sIxDeleteTS dtss db@TimeseriesDB{..} =
  foldl' (\acc (tag, ts) -> HM.update (fhm ts) tag acc) _sIx dhm
  where dhm = [(__tag, __timestamp) | dts@DTS{..} <- dtss]
        fhm ts im = let nim = IM.delete ts im in
                         if nim == IM.empty then Nothing
                                            else Just nim

vDeleteTS :: [DTS] -> TimeseriesDB -> V.Vector TS
vDeleteTS dtss db@TimeseriesDB{..} = V.force $ V.ifilter f _data'
  where f ix _ = not $ HM.member ix ixs
        ixs = force $ HM.fromList $ L.map ((,True) . flip unsafeIndexOf db . Right) dtss

vUpdateTS :: [TS] -> TimeseriesDB -> TimeseriesDB
vUpdateTS ts db =
  db & data' %~ V.force . V.modify (\v -> forM_ ts (\ts -> VM.write v (unsafeIndexOf (Left ts) db) ts))

queryF :: Monoid m => QueryModel -> (m -> a) -> (Ix -> m) -> ExceptQ (AggRes a m)
queryF qm = case qmToQT qm of
                TSQuery  -> queryTS
                TagQuery -> queryTag

queryVec :: Agg -> ExceptQ QueryR
queryVec agg = ask >>= \InternalQ{tdb=TimeseriesDB{..}} ->
                    let foldMMap' get to = return $ toQR $ get $ foldMap' (to . value) _data' in
                          case agg of
                            AvgAgg   -> handleAgg "Average failed." $ getAverage $ foldMap' (toAvg . value) _data'
                            CountAgg -> return $ toQR $ fromIntegral $ V.length _data'
                            SumAgg   -> foldMMap' getSum Sum
                            MinAgg   -> foldMMap' getMin Min
                            MaxAgg   -> foldMMap' getMax Max

queryDS :: ExceptQ QueryR
queryDS = ask
    >>= \InternalQ{qm=qm@Q{..},tdb=TimeseriesDB{..}}
        -> let toM to ix = to $! value $ getTS _data' ix
               simpleAgg get to = queryF qm get to <&> either toQR (toQRG get limit)
           in
               case aggFunc of
                    (Just AvgAgg) -> queryF qm getAverage (toM toAvg) >>=
                                                either (handleAgg "Average failed.")
                                                       (return . toQRG (fromMaybe 0 . getAverage) limit)
                    (Just SumAgg) ->  simpleAgg getSum (toM Sum)
                    (Just CountAgg) ->  simpleAgg getSum (const $! Sum 1)
                    (Just MinAgg) ->  simpleAgg getMin (toM Min)
                    (Just MaxAgg) ->  simpleAgg getMax (toM Max)
                    Nothing -> queryF qm id (\x -> [getTS _data' x]) <&> toCollR . maybe id take limit . fromLeft []

query :: ExceptQ QueryR
query = ask >>= \InternalQ{..}
                -> let (isOnlyAgg, agg) = onlyAgg qm in
                    if isOnlyAgg then queryVec agg
                                 else queryDS
