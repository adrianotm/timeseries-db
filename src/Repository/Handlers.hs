{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Repository.Handlers where

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

mapToTS :: V.Vector TS -> M.Map Tag Ix -> Collect TS
mapToTS d = foldMap $ toCollect . (V.!) d

insertTS :: [TS] -> Update TimeseriesDB ()
insertTS ts = do db@TimeseriesDB{..} <- get
                 let startIx = V.length data' in
                     put $ TimeseriesDB (evalState (IM.foldIx ts f tIx) startIx)
                                        (evalState (M.foldIx ts z sIx) startIx)
                                        (data' V.++ V.fromList ts)
                         where f tss = (timestamp tss, tag tss)
                               z = tag

searchTS :: Timestamp -> Query TimeseriesDB (Maybe [TS])
searchTS ts = do db@TimeseriesDB{..} <- ask
                 return $ getList . mapToTS data' <$> IM.lookup ts tIx

queryModelToRes :: QueryModel -> (IM.IntMap TagMap -> IM.IntMap TagMap)
queryModelToRes Q {gt = (Just gt), lt = (Just lt)} = IM.lookupGLT' False False gt lt
queryModelToRes Q {gt = (Just gt), le = (Just le)} = IM.lookupGLT' False True gt le
queryModelToRes Q {lt = (Just lt), ge = (Just ge)} = IM.lookupGLT' True False ge lt
queryModelToRes Q {ge = (Just ge), le = (Just le)} = IM.lookupGLT' True True ge le
queryModelToRes Q {gt = (Just gt)}                 = IM.lookupGT' False gt
queryModelToRes Q {ge = (Just ge)}                 = IM.lookupGT' True ge
queryModelToRes Q {lt = (Just lt)}                 = IM.lookupLT' False lt
queryModelToRes Q {le = (Just le)}                 = IM.lookupLT' True le
queryModelToRes Q {}                               = const IM.empty    --- returns error

filterTS' :: Maybe Tag -> (IM.IntMap TagMap -> IM.IntMap TagMap) -> Query TimeseriesDB [TS]
filterTS' Nothing f = ask <&> \db -> getList $ foldMap (mapToTS $ data' db) (f (tIx db))

filterTS' (Just tg) f = do db@TimeseriesDB{..} <- ask
                           return $ getList $ foldMap (\m -> case M.lookup tg m of
                                                             (Just ix) -> toCollect $ data' V.! ix
                                                             Nothing -> mempty)
                                                      (f tIx)

filterTS :: Maybe Tag
         -> QueryModel
         -> Query TimeseriesDB [TS]
filterTS tg qm = filterTS' tg $ queryModelToRes qm

getAllTS :: Query TimeseriesDB [TS]
getAllTS = filterTS' Nothing id

searchTag :: Tag -> Query TimeseriesDB [TS]
searchTag s = do db@TimeseriesDB{..} <- ask
                 return $ case M.lookup s sIx of
                            Just ixs -> getList $ foldMap (toCollect . (V.!) data') ixs
                            Nothing  -> []

clearTS :: Update TimeseriesDB ()
clearTS = put $ TimeseriesDB IM.empty M.empty V.empty

makeAcidic ''TimeseriesDB ['insertTS, 'getAllTS, 'clearTS, 'searchTS, 'filterTS, 'searchTag]
