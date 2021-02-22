{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
module Api where

import           Control.Exception.Base   (bracket)

import           Control.Monad.Reader
import           Data.Acid                as A
import           Data.Acid.Advanced       as AA
import           Data.Aeson               (FromJSON, ToJSON)
import           Data.Functor
import           Data.IntMap              as IM
import           Debug.Trace
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.API

import           Aggregates
import           Repository.Handlers
import           Repository.Model

debug = flip trace

type AcidReaderT = ReaderT (AcidState TimeseriesDB) Handler
type TSServer api = ServerT api AcidReaderT

type TimestampApi = Capture "ts" Timestamp :> Get '[JSON] [TS]

type TSQueryParams = ReqBody '[JSON] QueryModel :> Get '[JSON] [TS]
                :<|> Get '[JSON] [TS]

type TagApi = Capture "tagV" String :> ReqBody '[JSON] QueryModel :> Get '[JSON] [TS]
         :<|> Capture "tagV" String :> Get '[JSON] [TS]
         :<|> Capture "tagV" Int :> ReqBody '[JSON] QueryModel :> Get '[JSON] [TS]
         :<|> Capture "tagV" Int :> Get '[JSON] [TS]

type BasicTimeseriesApi =
   ReqBody '[JSON] [TS] :> Post '[JSON] [TS]
   :<|> ReqBody '[JSON] QueryModel :> Get '[JSON] [TS]
   :<|> Get '[JSON] [TS]
   :<|> Delete '[PlainText] NoContent

type TimeseriesApi =
  "timestamp" :> TimestampApi
   :<|> "tag" :> TagApi
   :<|> BasicTimeseriesApi

type API = "timeseries" :> TimeseriesApi

api :: Proxy API
api = Proxy

updateData :: [TS] -> AcidReaderT [TS]
updateData ts = (ask >>= flip update' (InsertTS ts)) $> ts

getData :: AcidReaderT [TS]
getData = ask >>= flip query' GetAllTS

clearData :: AcidReaderT NoContent
clearData = (ask >>= flip update' ClearTS) $> NoContent

findData :: Timestamp -> AcidReaderT [TS]
findData ts = ask >>= flip query' (SearchTS ts)
                  >>= \case Just r -> return r
                            Nothing -> throwError $ err404 { errBody = "Timestamp not found" }

filterData :: Maybe Tag
           -> QueryModel
           -> AcidReaderT [TS]
filterData Nothing qm  | emptyQM qm = getData
filterData tag q | illegalQM q = throwError $ err404 { errBody = "Illegal query "}
                 | otherwise = ask >>= flip query' (FilterTS tag q)

findTag :: Tag -> AcidReaderT [TS]
findTag s = ask >>= flip query' (SearchTag s)

timestampHandlers :: TSServer TimestampApi
timestampHandlers = findData

tagHandlers :: TSServer TagApi
tagHandlers = filterData . Just . Left
         :<|> findTag . Left
         :<|> filterData . Just . Right
         :<|> findTag . Right

tsHandlers :: TSServer TimeseriesApi
tsHandlers = timestampHandlers
        :<|> tagHandlers
        :<|> updateData
        :<|> filterData Nothing
        :<|> getData
        :<|> clearData

serverT :: TSServer API
serverT = tsHandlers

app :: AcidState TimeseriesDB -> Application
app db = serve api $ hoistServer api (`runReaderT` db) serverT
