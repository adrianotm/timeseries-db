{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}

module Repository.Queries
  ( Error,
    validInsert,
    validModify,
    tIxAppendTS,
    sIxAppendTS,
    tIxUpdateTS,
    sIxUpdateTS,
    tIxDeleteTS,
    sIxDeleteTS,
    query,
  )
where

import           Aggregates                  (Average, getAverage, handleAvg,
                                              toAvg, toCollR, toQR)
import           Control.DeepSeq             (NFData, force)
import           Control.Monad.Reader        (ask)
import           Data.Either                 (fromLeft)
import           Data.Foldable               (forM_)
import           Data.Function               ((&))
import           Data.Functor                ((<&>))
import           Data.List                   as L (delete, foldl', map, reverse,
                                                   sort, (\\))
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
                                              onlyAgg)
import           Repository.Queries.Tag      (queryTag)
import           Repository.Queries.TS       (queryTS)
import           Repository.Queries.Utils    (AggRes, ExceptQ,
                                              InternalQ (InternalQ, qm, tdb),
                                              QueryType (TSQuery, TagQuery),
                                              ofoldMap', qmToQT, toQRG)

type Error = String

errMsgModify :: Timestamp -> Tag -> Error
errMsgModify timestamp tag = "Timestamp = " ++ show timestamp ++ " and tag = " ++ show tag ++ " not found."

errMsgInsert :: TS -> Error
errMsgInsert TS {..} = "Timestamp = " ++ show timestamp ++ " and tag = " ++ show tag ++ " already exists."

validModify :: TagIndex -> [TS'] -> [Error]
validModify sIx = mapMaybe (\ts@TS' {..} -> maybe (Just $ errMsgModify timestamp' tag') (const Nothing) (IM.lookup timestamp' =<< HM.lookup tag' sIx))

validInsert :: TagIndex -> [TS] -> [Error]
validInsert sIx = mapMaybe (\ts@TS {..} -> const (Just $ errMsgInsert ts) =<< IM.lookup timestamp =<< HM.lookup tag sIx)

tIxAppendTS :: [TS] -> TimestampIndex -> TimestampIndex
tIxAppendTS tss im =
  foldl' (\acc ts@TS{..} -> IM.insertWith (++) timestamp [ts] acc) im tss

sIxAppendTS :: [TS] -> TagIndex -> TagIndex
sIxAppendTS tss m =
  foldl' f m tss
  where
    f acc ts@TS{..} =
      case HM.lookup tag acc of
        Nothing   -> HM.insert tag (IM.fromList [(timestamp, ts)]) acc
        (Just im) -> HM.insert tag (IM.insert timestamp ts im) acc

tIxUpdateTS :: [TS] -> TimestampIndex -> TimestampIndex
tIxUpdateTS tss im =
  foldl' (\acc ts@TS{..} -> IM.update (f ts) timestamp acc) im tss
  where
    f ts l = Just $ ts : L.delete ts l

sIxUpdateTS :: [TS] -> TagIndex -> TagIndex
sIxUpdateTS tss m =
  foldl' (\acc ts@TS{..} -> HM.update (fhm ts) tag acc) m tss
  where
    fhm ts@TS{..} im = Just $ IM.insert timestamp ts im

tIxDeleteTS :: [TS'] -> TimestampIndex -> TimestampIndex
tIxDeleteTS dtss im =
  foldl' (\acc TS'{..} -> IM.update (f (TS timestamp' tag' 0)) timestamp' acc) im dtss
  where
    f ts l = case L.delete ts l of
      []  -> Nothing
      tss -> Just tss

sIxDeleteTS :: [TS'] -> TagIndex -> TagIndex
sIxDeleteTS dtss m =
  foldl' (\acc TS'{..} -> HM.update (fhm timestamp') tag' acc) m dtss
  where
    fhm ts im =
      let nim = IM.delete ts im
       in if nim == IM.empty
            then Nothing
            else Just nim

queryF :: (NFData m, Monoid m) => QueryModel -> (m -> a) -> (TS -> m) -> ExceptQ (AggRes a m)
queryF qm = case qmToQT qm of
  TSQuery  -> queryTS
  TagQuery -> queryTag

-- -- Aggergate the vector
-- queryVec :: Agg -> ExceptQ QueryR
-- queryVec agg =
--   ask >>= \InternalQ {tdb = TimeseriesDB {..}} ->
--     let foldMMap' get to = return $ toQR $ get $ ofoldMap' to _dataV'
--      in case agg of
--           AvgAgg -> handleAvg "Average failed." $ getAverage $ ofoldMap' toAvg _dataV'
--           CountAgg -> return $ toQR $ fromIntegral $ V.length _data'
--           SumAgg -> foldMMap' getSum Sum
--           MinAgg -> foldMMap' getMin Min
--           MaxAgg -> foldMMap' getMax Max

-- Query the indexes
query :: ExceptQ QueryR
query =
  ask
    >>= \InternalQ {qm = qm@Q {..}, tdb = tdb@TimeseriesDB {..}} ->
      let toM to TS{..} = to value
          simpleAgg get toM = queryF qm get toM <&> either toQR (toQRG get limit)
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
              queryF qm id (: [])
                <&> toCollR . maybe id take limit . fromLeft []

-- Decides whether to use the vector directly or the indexes
-- query :: ExceptQ QueryR
-- query =
--   ask >>= \InternalQ {..} ->
--     let (isOnlyAgg, agg) = onlyAgg qm
--      in queryDS
