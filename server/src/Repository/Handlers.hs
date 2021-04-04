{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Repository.Handlers
  ( InsertTS(InsertTS)
  , UpdateTS(UpdateTS)
  , ClearTS(ClearTS)
  , FilterTS(FilterTS))
  where

import           Control.Monad.Except      (forM_, runExceptT)
import           Control.Monad.Reader      (ask, runReader, runReaderT)
import           Control.Monad.State       (MonadState, evalState, get, put)
import           Data.Acid                 (Query, Update, makeAcidic)
import           Data.Functor              (($>), (<&>))
import           Data.Maybe                (fromMaybe)
import qualified Data.Vector               as V
import qualified Data.Vector.Mutable       as VM
import qualified DataS.HashMap             as HM
import qualified DataS.IntMap              as IM
import           GHC.Compact               (Compact, compact, compactAdd,
                                            getCompact)
import           Repository.Model          (DB (..), DTS, QueryModel (..),
                                            QueryR, TS, Tag, TimeseriesDB (..),
                                            Timestamp, data')
import           Repository.Queries        (Error, query, sIxAppendTS,
                                            sIxDeleteTS, tIxAppendTS,
                                            tIxDeleteTS, unsafeIndexOf,
                                            vDeleteTS, vUpdateTS, validDelete,
                                            validInsert, validUpdate)
import           Repository.Queries.Shared (InternalQ (InternalQ))
import           System.IO.Unsafe          (unsafePerformIO)

getTSDB :: Monad m => DB -> m TimeseriesDB
getTSDB (DB a) = return $ getCompact a

unsafeCompactAddDB :: Compact TimeseriesDB -> TimeseriesDB -> DB
unsafeCompactAddDB c = DB . unsafePerformIO . compactAdd c
{-# NOINLINE unsafeCompactAddDB #-}

unsafeCompactDB :: TimeseriesDB -> DB
unsafeCompactDB = DB . unsafePerformIO . compact
{-# NOINLINE unsafeCompactDB #-}

insertTS :: [TS] -> Update DB [Error]
insertTS ts = do DB cdb <- get
                 let db@TimeseriesDB{..} = getCompact cdb in
                   case validInsert db ts of
                     [] -> let startIx = V.length _data' in
                                   put (unsafeCompactAddDB cdb
                                          $ TimeseriesDB (tIxAppendTS ts _tIx startIx)
                                                         (sIxAppendTS ts _sIx startIx)
                                                         (V.force $ _data' V.++ V.fromList ts)) $> []
                     errors -> return $ take 10 errors

updateTS :: [TS] -> Update DB [Error]
updateTS ts = get >>= \(DB cdb) ->
                 let db@TimeseriesDB{..} = getCompact cdb in
                      case validUpdate db ts of
                           []     -> put (unsafeCompactAddDB cdb $ vUpdateTS ts db) $> []
                           errors -> return $ take 10 errors

clearTS :: [DTS] -> Update DB [Error]
clearTS dts = get >>=
               \(DB cdb) ->
                 let db@TimeseriesDB{..} = getCompact cdb in
                      case dts of
                        [] -> put (unsafeCompactAddDB cdb $ TimeseriesDB IM.empty HM.empty V.empty) $> []
                        dtss -> get >>= getTSDB
                                        >>= \db@TimeseriesDB{..}
                                            -> case validDelete db dtss of
                                                []     -> put (unsafeCompactAddDB cdb $
                                                                 TimeseriesDB (tIxDeleteTS dtss db)
                                                                              (sIxDeleteTS dtss db)
                                                                              (vDeleteTS dtss db)) $> []
                                                errors -> return $ take 10 errors

filterTS :: QueryModel
         -> Query DB (Either Error QueryR)
filterTS qm@Q{..} = ask >>= getTSDB <&> runReader (runExceptT query) . InternalQ qm

makeAcidic ''DB ['insertTS, 'clearTS, 'filterTS, 'updateTS]
