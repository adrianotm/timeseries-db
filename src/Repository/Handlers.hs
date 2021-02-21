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
import qualified Data.Sequence        as S
import qualified Data.Vector          as V
import qualified IntMap               as IM
import qualified Map                  as M

import           Repository.Model

mapToTS :: V.Vector TS -> M.Map Tag Ix -> [TS]
mapToTS d = M.foldl (\acc ix -> (d V.! ix) : acc) []

getAllTS :: Query TimeseriesDB [TS]
getAllTS = do db@TimeseriesDB{..} <- ask
              return $ IM.foldrWithKey (f db) [] tIx
            where f db t m acc = mapToTS (data' db) m ++ acc

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
                 return $ fmap (mapToTS data') (IM.lookup ts tIx)

filterTS' :: Maybe Tag -> (IM.IntMap TagMap -> [TagMap]) -> Query TimeseriesDB [TS]
filterTS' Nothing f = do db@TimeseriesDB{..} <- ask
                         return $ concatMap (mapToTS data') (f tIx)

filterTS' (Just tg) f = do db@TimeseriesDB{..} <- ask
                           return $ foldr (\m acc -> case M.lookup tg m of
                                                       (Just ix) -> data' V.! ix : acc
                                                       Nothing -> acc
                                          ) [] (f tIx)

queryModelToRes :: QueryModel -> (IM.IntMap TagMap -> [TagMap])
queryModelToRes Q {gt = (Just gt), lt = (Just lt)} = IM.lookupGLT gt lt
queryModelToRes Q {gt = (Just gt), le = (Just le)} = IM.lookupGTLE gt le
queryModelToRes Q {lt = (Just lt), ge = (Just ge)} = IM.lookupGELT ge lt
queryModelToRes Q {ge = (Just ge), le = (Just le)} = IM.lookupGELE ge le
queryModelToRes Q {gt = (Just gt)}                 = IM.lookupGT gt
queryModelToRes Q {ge = (Just ge)}                 = IM.lookupGE ge
queryModelToRes Q {lt = (Just lt)}                 = IM.lookupLT lt
queryModelToRes Q {le = (Just le)}                 = IM.lookupLE le
queryModelToRes Q {}                               = const []    --- returns error

filterTS :: Maybe Tag
         -> QueryModel
         -> Query TimeseriesDB [TS]
filterTS tg qm = filterTS' tg $ queryModelToRes qm

searchTag :: Either String Int -> Query TimeseriesDB [TS]
searchTag s = do db@TimeseriesDB{..} <- ask
                 return $ case M.lookup s sIx of
                            Just ixs -> map (data' V.!) (toList ixs)
                            Nothing  -> []

clearTS :: Update TimeseriesDB ()
clearTS = put $ TimeseriesDB IM.empty M.empty V.empty

makeAcidic ''TimeseriesDB ['insertTS, 'getAllTS, 'clearTS, 'searchTS, 'filterTS, 'searchTag]
