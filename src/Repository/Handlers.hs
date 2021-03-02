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
import           Data.Foldable
import           Data.Function
import           Data.Functor
import           Data.Maybe
import qualified Data.Vector               as V
import qualified Data.Vector.Mutable       as VM
import qualified DataS.IntMap              as IM
import qualified DataS.Map                 as M

import           Aggregates
import           Repository.Model
import           Repository.Queries
import           Repository.Queries.Shared
import           Repository.Utils

updateTS :: [TS] -> Update TimeseriesDB [Error]
updateTS ts = get >>= \db -> case validUpdate db ts of
                               [] -> put (db & data' %~ V.modify (\v -> forM_ ts (\ts -> VM.write v (unsafeIndexOf ts db) ts))) $> []
                               errors -> return errors

insertTS :: [TS] -> Update TimeseriesDB [Error]
insertTS ts = do db@TimeseriesDB{..} <- get
                 case validInsert db ts of
                   [] -> let startIx = V.length _data' in
                                 put (TimeseriesDB (evalState (IM.foldIx ts f _tIx) startIx)
                                                   (evalState (M.foldIx ts z _sIx) startIx)
                                                   (_data' V.++ V.fromList ts)) $> []
                                where f tss = (timestamp tss, tag tss)
                                      z = tag
                   errors -> return errors

filterTS :: QueryModel
         -> Query TimeseriesDB (Either Error QueryR)
filterTS qm@Q{..} = ask <&> \db -> runReader (runExceptT query) $ InternalQ qm db

clearTS :: Update TimeseriesDB ()
clearTS = put $ TimeseriesDB IM.empty M.empty V.empty

allTimestamps :: Bool -> Query TimeseriesDB (Either Error [Timestamp])
allTimestamps bounded = ask <&> \db ->
                                  if bounded
                                    then fromMaybe (Left "No data.") $
                                          (\(min, _) (max, _) -> Right [min, max])
                                            <$> IM.lookupMin (_tIx db) <*> IM.lookupMax (_tIx db)
                                    else Right $ IM.keys $ _tIx db

allTags :: Query TimeseriesDB [Tag]
allTags = ask <&> M.keys . _sIx

makeAcidic ''TimeseriesDB ['insertTS, 'clearTS, 'filterTS, 'updateTS, 'allTimestamps, 'allTags]
