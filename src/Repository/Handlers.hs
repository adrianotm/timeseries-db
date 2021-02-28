{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Repository.Handlers where

import           Control.Applicative
import           Control.Lens              ((%~), (.~))
import           Control.Monad.Except
import           Control.Monad.Reader      (ask, runReader, runReaderT)
import           Control.Monad.State       (MonadState, evalState, get, put)
import           Data.Acid                 (Query, Update, makeAcidic)
import qualified Data.DList                as DL
import           Data.Either
import           Data.Foldable
import           Data.Function
import           Data.Functor
import           Data.Maybe
import qualified Data.Sequence             as S
import qualified Data.Vector               as V
import qualified Data.Vector.Mutable       as VM
import qualified DataS.IntMap              as IM
import qualified DataS.Map                 as M
import           Debug.Trace

import           Aggregates
import           Repository.Model
import           Repository.Queries
import           Repository.Queries.Shared

debug = flip trace

unsafeIndexOf :: TS -> TimeseriesDB -> Ix
unsafeIndexOf TS{..} TimeseriesDB{..} = (_tIx IM.! timestamp) M.! tag

errMsgUpdate :: TS -> String
errMsgUpdate TS{..} = "Timestamp=" ++ show timestamp ++ " and tag=" ++ show tag ++ " not found."

errMsgInsert :: TS -> String
errMsgInsert TS{..} = "Timestamp=" ++ show timestamp ++ " and tag=" ++ show tag ++ " already exists."

validUpdate :: TimeseriesDB -> [TS] -> Maybe [String]
validUpdate TimeseriesDB{..} tss = forM tss (\ts@TS{..} -> maybe (Just $ errMsgUpdate ts) (const Nothing) (M.lookup tag =<< IM.lookup timestamp _tIx))

validInsert :: TimeseriesDB -> [TS] -> Maybe [String]
validInsert TimeseriesDB{..} tss = forM tss (\ts@TS{..} -> const (Just $ errMsgInsert ts) =<< M.lookup tag =<< IM.lookup timestamp _tIx)

updateTS :: [TS] -> Update TimeseriesDB (Maybe [String])
updateTS ts = get >>= \db -> case validUpdate db ts of
                               (Just s) -> return $ Just s
                               Nothing -> put (db & data' %~ V.modify (\v -> forM_ ts (\ts -> VM.write v (unsafeIndexOf ts db) ts))) $> Nothing

insertTS :: [TS] -> Update TimeseriesDB (Maybe [String])
insertTS ts = do db@TimeseriesDB{..} <- get
                 case validInsert db ts of
                   (Just s) -> return $ Just s
                   Nothing -> let startIx = V.length _data' in
                                 put (TimeseriesDB (evalState (IM.foldIx ts f _tIx) startIx)
                                                   (evalState (M.foldIx ts z _sIx) startIx)
                                                   (_data' V.++ V.fromList ts)) $> Nothing
                                where f tss = (timestamp tss, tag tss)
                                      z = tag

filterTS :: QueryModel
         -> Query TimeseriesDB (Either String QueryR)
filterTS qm@Q{..} = ask <&> \db -> runReader (runExceptT query) $ InternalQ qm db

getAllTS :: Query TimeseriesDB [TS]
getAllTS = ask <&> DL.toList . getList . foldMap' toCollect . _data'

clearTS :: Update TimeseriesDB ()
clearTS = put $ TimeseriesDB IM.empty M.empty V.empty

makeAcidic ''TimeseriesDB ['insertTS, 'getAllTS, 'clearTS, 'filterTS, 'updateTS]
