{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}

module Repository.Queries
  ( Error,
    unsafeIndexOf,
    validInsert,
    validModify,
    tIxAppendTS,
    sIxAppendTS,
    tIxDeleteTS,
    sIxDeleteTS,
    vDeleteTS,
    vUpdateTS,
    query,
  )
where

import           Aggregates                  (getAverage, toAvg)
import           Control.DeepSeq             (NFData, force)
import           Control.Lens                ((%~))
import           Control.Monad.Reader        (ask)
import           Data.Either                 (fromLeft)
import           Data.Foldable               (forM_)
import           Data.Function               ((&))
import           Data.Functor                ((<&>))
import           Data.List                   as L (delete, foldl', map)
import           Data.Maybe                  (fromMaybe, mapMaybe)
import           Data.Monoid                 (Sum (Sum, getSum))
import           Data.Semigroup              (Max (Max, getMax),
                                              Min (Min, getMin))
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as UV
import qualified Data.Vector.Unboxed.Mutable as UVM
import qualified DataS.HashMap               as HM
import qualified DataS.IntMap                as IM
import           Repository.Model            (Agg (..), Ix, QueryModel (..),
                                              QueryR (..), TS (..), TS' (..),
                                              Tag, TagIndex, TimeseriesDB (..),
                                              Timestamp, TimestampIndex, Val,
                                              dataV', onlyAgg)
import           Repository.Queries.Tag      (queryTag)
import           Repository.Queries.TS       (queryTS)
import           Repository.Queries.Utils    (AggRes, ExceptQ,
                                              InternalQ (InternalQ, qm, tdb),
                                              QueryType (TSQuery, TagQuery),
                                              handleAvg, makeTS, ofoldMap',
                                              qmToQT, toCollR, toQR, toQRG)

type Error = String

-- | Returns the position of TS or TS' in the vector
unsafeIndexOf :: Either TS TS' -> TimeseriesDB -> Ix
unsafeIndexOf (Left TS {..}) TimeseriesDB {..} = (_sIx HM.! tag) IM.! timestamp
unsafeIndexOf (Right TS' {..}) TimeseriesDB {..} = (_sIx HM.! tag') IM.! timestamp'

-- | Error message when a timestamp/tag combination doesn't exist in the database
errMsgModify :: Timestamp -> Tag -> Error
errMsgModify timestamp tag = "Timestamp = " ++ show timestamp ++ " and tag = " ++ show tag ++ " not found."

-- | Error message when a timestamp/tag combination exists in the database
errMsgInsert :: TS -> Error
errMsgInsert TS {..} = "Timestamp = " ++ show timestamp ++ " and tag = " ++ show tag ++ " already exists."

-- | For a given list of data that needs to be updated or deleted,
--   return a error message for each entrie that doesn't exist in the database
--   if there are no errors, it means that the modification is valid
validModify :: TagIndex -> [TS'] -> [Error]
validModify sIx = mapMaybe (\TS' {..} -> maybe (Just $ errMsgModify timestamp' tag') (const Nothing) (IM.lookup timestamp' =<< HM.lookup tag' sIx))

-- | For a given list of data that needs to be inserted,
--   return a error message for each entrie that already exists in the database
--   if there are no errors, it means that the insertion is valid
validInsert :: TagIndex -> [TS] -> [Error]
validInsert sIx = mapMaybe (\ts@TS {..} -> const (Just $ errMsgInsert ts) =<< IM.lookup timestamp =<< HM.lookup tag sIx)

-- | Update the timestamp index with new data
tIxAppendTS :: [TS] -> TimestampIndex -> Ix -> TimestampIndex
tIxAppendTS tss im ix =
  foldl' (\acc (ts, inx) -> IM.insertWith (++) ts inx acc) im appIM
  where
    appIM = [(timestamp, [i]) | TS {..} <- tss | i <- [ix ..]]

-- | Update the tag/timestamp index with new data
sIxAppendTS :: [TS] -> TagIndex -> Ix -> TagIndex
sIxAppendTS ts m startIx =
  foldl' f m appIM
  where
    appIM = [(tag, timestamp, i) | TS {..} <- ts | i <- [startIx ..]]
    f acc (tag, timestamp, ix) =
      case HM.lookup tag acc of
        Nothing   -> HM.insert tag (IM.fromList [(timestamp, ix)]) acc
        (Just im) -> HM.insert tag (IM.insert timestamp ix im) acc

-- | Delete data from the timestamp index
tIxDeleteTS :: [TS'] -> TimeseriesDB -> TimestampIndex
tIxDeleteTS dtss db@TimeseriesDB {..} =
  foldl' (\acc (ts, ix) -> IM.update (f ix) ts acc) _tIx dts
  where
    dts = [(timestamp', unsafeIndexOf (Right dts') db) | dts'@TS' {..} <- dtss]
    f ix l = case L.delete ix l of
      []  -> Nothing
      ixs -> Just ixs

-- | Delete data from the tag/timestamp index
sIxDeleteTS :: [TS'] -> TimeseriesDB -> TagIndex
sIxDeleteTS dtss TimeseriesDB {..} =
  foldl' (\acc (tag, ts) -> HM.update (fhm ts) tag acc) _sIx dhm
  where
    dhm = [(tag', timestamp') | TS' {..} <- dtss]
    fhm ts im =
      let nim = IM.delete ts im
       in if nim == IM.empty
            then Nothing
            else Just nim

-- | Delete data from the vectors
vDeleteTS :: [TS'] -> TimeseriesDB -> (V.Vector TS', UV.Vector Val)
vDeleteTS dtss db@TimeseriesDB {..} = (V.force $ V.ifilter f _data', UV.force $ UV.ifilter f _dataV')
  where
    f ix _ = not $ HM.member ix ixs
    ixs = force $ HM.fromList $ L.map ((,True) . flip unsafeIndexOf db . Right) dtss

-- | Update data in the vector by mutating it
vUpdateTS :: [TS] -> TimeseriesDB -> TimeseriesDB
vUpdateTS ts db =
  db & dataV' %~ UV.force . UV.modify (\v -> forM_ ts (\ts -> UVM.write v (unsafeIndexOf (Left ts) db) (value ts)))

-- | Decide whether to query the timestamp index or the tag/timestamp index
queryF :: (NFData m, Monoid m) => QueryModel -> (m -> a) -> (Ix -> m) -> ExceptQ (AggRes a m)
queryF qm = case qmToQT qm of
  TSQuery  -> queryTS
  TagQuery -> queryTag

-- | Query the indexes
--   aggregate data using different Monoids depending on the aggregation function
queryDS :: ExceptQ QueryR
queryDS =
  ask
    >>= \InternalQ {qm = qm@Q {..}, tdb = tdb@TimeseriesDB {..}} ->
      let toM to ix = to $! UV.unsafeIndex _dataV' ix
          simpleAgg getM toM = queryF qm getM toM <&> either toQR (toQRG getM limit)
       in case aggFunc of
            (Just AvgAgg) ->
              queryF qm getAverage (toM toAvg)
                >>= either
                  (handleAvg "Average failed.")
                  (return . toQRG (fromMaybe 0 . getAverage) limit)
            (Just SumAgg) -> simpleAgg getSum (toM Sum)
            (Just CountAgg) -> simpleAgg getSum (const $! Sum 1)
            (Just MinAgg) -> simpleAgg getMin (toM Min)
            (Just MaxAgg) -> simpleAgg getMax (toM Max)
            Nothing ->
              queryF qm id (\ix -> [makeTS tdb ix])
                <&> toCollR . maybe id take limit . fromLeft []

-- | Aggergate the vector directly
queryVec :: Agg -> ExceptQ QueryR
queryVec agg =
  ask >>= \InternalQ {tdb = TimeseriesDB {..}} ->
    let foldMMap' get to = return $ toQR $ get $ ofoldMap' to _dataV'
     in case agg of
          AvgAgg -> handleAvg "Average failed." $ getAverage $ ofoldMap' toAvg _dataV'
          CountAgg -> return $ toQR $ fromIntegral $ V.length _data'
          SumAgg -> foldMMap' getSum Sum
          MinAgg -> foldMMap' getMin Min
          MaxAgg -> foldMMap' getMax Max

-- | Decide whether to use the indexes or the vector directly
--   if only an 'aggFunc' is present in the query, then use the vector
--   otherwise use the indexes
query :: ExceptQ QueryR
query =
  ask >>= \InternalQ {..} ->
    let (isOnlyAgg, agg) = onlyAgg qm
     in if isOnlyAgg
          then queryVec agg
          else queryDS
