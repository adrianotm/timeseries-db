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
import           Control.Monad.Except
import           Control.Monad.Reader      (ask, runReader, runReaderT)
import           Control.Monad.State       (MonadState, evalState, get, put)
import           Data.Acid                 (Query, Update, makeAcidic)
import qualified Data.DList                as DL
import           Data.Foldable
import           Data.Functor
import           Data.Maybe
import qualified Data.Sequence             as S
import qualified Data.Vector               as V
import qualified DataS.IntMap              as IM
import qualified DataS.Map                 as M
import           Debug.Trace

import           Aggregates
import           Repository.Model
import           Repository.Queries
import           Repository.Queries.Shared

debug = flip trace

validInsert :: TimeseriesDB -> [TS] -> Bool
validInsert TimeseriesDB{..} = all (\TS{..} -> isNothing $ M.lookup tag =<< IM.lookup timestamp tIx)

insertTS :: [TS] -> Update TimeseriesDB (Either String ())
insertTS ts = do db@TimeseriesDB{..} <- get
                 if not $ validInsert db ts
                    then return $ Left "Duplicate timestamp and tag."
                    else let startIx = V.length data' in
                        do put $ TimeseriesDB (evalState (IM.foldIx ts f tIx) startIx)
                                              (evalState (M.foldIx ts z sIx) startIx)
                                              (data' V.++ V.fromList ts)
                           return $ Right ()
                 where f tss = (timestamp tss, tag tss)
                       z = tag

filterTS :: QueryModel
         -> Query TimeseriesDB (Either String QueryR)
filterTS qm@Q{..} = ask <&> \db -> runReader (runExceptT query) $ InternalQ qm db

getAllTS :: Query TimeseriesDB [TS]
getAllTS = ask <&> DL.toList . getList . foldMap' toCollect . data'

clearTS :: Update TimeseriesDB ()
clearTS = put $ TimeseriesDB IM.empty M.empty V.empty

makeAcidic ''TimeseriesDB ['insertTS, 'getAllTS, 'clearTS, 'filterTS]
