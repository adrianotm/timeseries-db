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
                                           Timestamp)
import           Repository.Queries       (Error, query, sIxAppendTS,
                                           sIxDeleteTS, sIxUpdateTS,
                                           tIxAppendTS, tIxDeleteTS,
                                           tIxUpdateTS, validInsert,
                                           validModify)
import           Repository.Queries.Utils (InternalQ (InternalQ), simpleTS)
import           System.IO.Unsafe         (unsafePerformIO)

insertTS :: [TS] -> Update TimeseriesDB [Error]
insertTS ts = do
  db@TimeseriesDB {..} <- get
  case validInsert sIx ts of
    [] -> do
      put $
        force
          ( TimeseriesDB
              (tIxAppendTS ts tIx)
              (sIxAppendTS ts sIx)
          )
      return []
    errors -> return $ take 10 errors

updateTS :: [TS] -> Update TimeseriesDB [Error]
updateTS ts =
  get >>= \db@TimeseriesDB {..} ->
    case validModify sIx $ map simpleTS ts of
      [] -> do
        put $
          force
            ( TimeseriesDB
                (tIxUpdateTS ts tIx)
                (sIxUpdateTS ts sIx)
            )
        return []
      errors -> return $ take 10 errors

clearTS :: [TS'] -> Update TimeseriesDB [Error]
clearTS dts = case dts of
  [] -> put (TimeseriesDB IM.empty HM.empty) $> []
  dtss ->
    get >>= \db@TimeseriesDB {..} ->
      case validModify sIx dtss of
        [] -> do
          put
            ( force $
                TimeseriesDB
                  (tIxDeleteTS dtss tIx)
                  (sIxDeleteTS dtss sIx)
            )
          return []
        errors -> return $ take 10 errors

filterTS ::
  QueryModel ->
  Query TimeseriesDB (Either Error QueryR)
filterTS qm@Q {..} = ask <&> runReader (runExceptT query) . InternalQ qm

makeAcidic ''TimeseriesDB ['insertTS, 'clearTS, 'filterTS, 'updateTS]
