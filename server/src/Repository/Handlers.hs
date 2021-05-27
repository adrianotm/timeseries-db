{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Repository.Handlers
  ( InsertTS (InsertTS),
    UpdateTS (UpdateTS),
    ClearTS (ClearTS),
    FilterTS (FilterTS),
  )
where

import           Control.DeepSeq          (force)
import           Control.Monad.Except     (forM_, runExceptT)
import           Control.Monad.Reader     (ask, runReader, runReaderT)
import           Control.Monad.State      (MonadState, evalState, get, put)
import           Data.Acid                (Query, Update, makeAcidic)
import           Data.Functor             (($>), (<&>))
import           Data.Maybe               (fromMaybe)
import qualified Data.Vector              as V
import qualified Data.Vector.Mutable      as VM
import qualified Data.Vector.Unboxed      as UV
import qualified DataS.HashMap            as HM
import qualified DataS.IntMap             as IM
import           Repository.Model         (QueryModel (..), QueryR, TS (..),
                                           TS' (..), Tag, TimeseriesDB (..),
                                           Timestamp, data')
import           Repository.Queries       (Error, query, sIxAppendTS,
                                           sIxDeleteTS, tIxAppendTS,
                                           tIxDeleteTS, unsafeIndexOf,
                                           vDeleteTS, vUpdateTS, validInsert,
                                           validModify)
import           Repository.Queries.Utils (InternalQ (InternalQ), simpleTS)
import           System.IO.Unsafe         (unsafePerformIO)

-- | Insert new data
-- if some data already exists, return a list of errors
-- else proceed with inserting and return an empty list
insertTS :: [TS] -> Update TimeseriesDB [Error]
insertTS ts = do
  db@TimeseriesDB {..} <- get
  case validInsert _sIx ts of
    [] ->
      let startIx = V.length _data'
       in put
            ( force $
                TimeseriesDB
                  (tIxAppendTS ts _tIx startIx)
                  (sIxAppendTS ts _sIx startIx)
                  (V.force $ _data' V.++ V.fromList (map simpleTS ts))
                  (UV.force $ _dataV' UV.++ UV.fromList (map value ts))
            )
            $> []
    errors -> return $ take 10 errors

-- | Update data
-- if some data doesn't exist, return a list of errors
-- else proceed with updating and return an empty list
updateTS :: [TS] -> Update TimeseriesDB [Error]
updateTS ts =
  get >>= \db@TimeseriesDB {..} ->
    case validModify _sIx $ map simpleTS ts of
      []     -> put (vUpdateTS ts db) $> []
      errors -> return $ take 10 errors

-- | Delete data
-- if an empty list is passed, clear the whole database
-- else if some data doesn't exist, return a list of errors
-- else proceed with deleting data and return an empty list
clearTS :: [TS'] -> Update TimeseriesDB [Error]
clearTS dts = case dts of
  [] -> put (TimeseriesDB IM.empty HM.empty V.empty UV.empty) $> []
  dtss ->
    get >>= \db@TimeseriesDB {..} ->
      case validModify _sIx dtss of
        [] ->
          put
            ( force $
                TimeseriesDB
                  (tIxDeleteTS dtss db)
                  (sIxDeleteTS dtss db)
                  newData
                  newDataV
            )
            $> []
          where
            (newData, newDataV) = vDeleteTS dtss db
        errors -> return $ take 10 errors

-- | Query data
-- returns either an error or the query result
filterTS ::
  QueryModel ->
  Query TimeseriesDB (Either Error QueryR)
filterTS qm@Q {..} = ask <&> runReader (runExceptT query) . InternalQ qm

makeAcidic ''TimeseriesDB ['insertTS, 'clearTS, 'filterTS, 'updateTS]
