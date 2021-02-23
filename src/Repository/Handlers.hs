{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Repository.Handlers where

import           Control.Applicative
import           Control.Monad.Reader (ask)
import           Control.Monad.State  (MonadState, evalState, get, put)
import           Data.Acid            (Query, Update, makeAcidic)
import           Data.Foldable
import           Data.Functor
import qualified Data.Sequence        as S
import qualified Data.Vector          as V
import qualified IntMap               as IM
import qualified Map                  as M

import           Aggregates
import           Repository.Model

qmToF :: QueryModel -> (IM.IntMap TagMap -> IM.IntMap TagMap)
qmToF Q {gt = (Just gt), lt = (Just lt)} = IM.lookupGLT' False False gt lt
qmToF Q {gt = (Just gt), le = (Just le)} = IM.lookupGLT' False True gt le
qmToF Q {lt = (Just lt), ge = (Just ge)} = IM.lookupGLT' True False ge lt
qmToF Q {ge = (Just ge), le = (Just le)} = IM.lookupGLT' True True ge le
qmToF Q {gt = (Just gt)}                 = IM.lookupGT' False gt
qmToF Q {ge = (Just ge)}                 = IM.lookupGT' True ge
qmToF Q {lt = (Just lt)}                 = IM.lookupLT' False lt
qmToF Q {le = (Just le)}                 = IM.lookupLT' True le
qmToF Q {}                               = id         --- returns error

mapToTS :: Maybe Tag -> V.Vector TS -> M.Map Tag Ix -> Collect TS
mapToTS Nothing d m   = foldMap' (toCollect . (V.!) d) m
mapToTS (Just tg) d m = maybe mempty (toCollect . (V.!) d) (M.lookup tg m)

insertTS :: [TS] -> Update TimeseriesDB ()
insertTS ts = do db@TimeseriesDB{..} <- get
                 let startIx = V.length data' in
                     put $ TimeseriesDB (evalState (IM.foldIx ts f tIx) startIx)
                                        (evalState (M.foldIx ts z sIx) startIx)
                                        (data' V.++ V.fromList ts)
                         where f tss = (timestamp tss, tag tss)
                               z = tag

searchTS :: Timestamp -> Query TimeseriesDB (Maybe [TS])
searchTS ts = do TimeseriesDB{..} <- ask
                 return $ getList . mapToTS Nothing data' <$> IM.lookup ts tIx

filterTS' :: Maybe Tag -> (IM.IntMap TagMap -> IM.IntMap TagMap) -> Query TimeseriesDB [TS]
filterTS' tg f = ask <&> \db -> getList $ foldMap' (mapToTS tg $ data' db) (f (tIx db))

filterTS :: QueryModel
         -> Query TimeseriesDB [TS]
filterTS qm = case justTag qm of
                Just tg -> searchTag tg
                Nothing -> filterTS' (tagEq qm) $ qmToF qm

getAllTS :: Query TimeseriesDB [TS]
getAllTS = filterTS' Nothing id

searchTag :: Tag -> Query TimeseriesDB [TS]
searchTag s = do db@TimeseriesDB{..} <- ask
                 return $ case M.lookup s sIx of
                            Just ixs -> getList $ foldMap' (toCollect . (V.!) data') ixs
                            Nothing  -> []

clearTS :: Update TimeseriesDB ()
clearTS = put $ TimeseriesDB IM.empty M.empty V.empty

makeAcidic ''TimeseriesDB ['insertTS, 'getAllTS, 'clearTS, 'searchTS, 'filterTS, 'searchTag]
