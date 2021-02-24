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

import           Control.Exception.Base     (bracket)

import           Control.Monad.Reader
import           Data.Acid                  as A
import           Data.Acid.Advanced         as AA
import           Data.Aeson                 (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Functor
import           Data.IntMap                as IM
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


type TimeseriesApi =
   ReqBody '[JSON] [TS] :> Post '[JSON] [TS]
   :<|> ReqBody '[JSON] QueryModel :> Get '[JSON] QueryR
   :<|> Get '[JSON] [TS]
   :<|> Delete '[PlainText] NoContent

type API = "timeseries" :> TimeseriesApi

api :: Proxy API
api = Proxy

updateData :: [TS] -> AcidReaderT [TS]
updateData ts = (ask >>= flip update' (InsertTS ts)) $> ts

getData :: AcidReaderT [TS]
getData = ask >>= flip query' GetAllTS

clearData :: AcidReaderT NoContent
clearData = (ask >>= flip update' ClearTS) $> NoContent

queryData :: QueryModel
           -> AcidReaderT QueryR
queryData qm  | emptyQM qm = toCollR <$> getData
              | illegalQM qm = throwError $ err404 { errBody = "Illegal query "}
              | otherwise = ask >>= flip query' (FilterTS qm) >>= either
                                                                  (\m -> throwError $ err404 { errBody = C.pack m })
                                                                  return

tsHandlers :: TSServer TimeseriesApi
tsHandlers = updateData
        :<|> queryData
        :<|> getData
        :<|> clearData

serverT :: TSServer API
serverT = tsHandlers

app :: AcidState TimeseriesDB -> Application
app db = serve api $ hoistServer api (`runReaderT` db) serverT
