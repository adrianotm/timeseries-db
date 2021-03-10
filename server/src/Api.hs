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
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.API

import           Aggregates
import           Repository.Handlers
import           Repository.Model
import           Repository.Utils

type AcidReaderT = ReaderT (AcidState TimeseriesDB) Handler
type TSServer api = ServerT api AcidReaderT

type TimeseriesApi =
   ReqBody '[JSON] [TS] :> Post '[JSON] ()
   :<|> ReqBody '[JSON] [TS] :> Put '[JSON] ()
   :<|> ReqBody '[JSON] QueryModel :> Get '[JSON] QueryR
   :<|> Get '[JSON] [TS]
   :<|> ReqBody '[JSON] [DTS] :> Delete '[JSON] ()
   :<|> Delete '[JSON] ()
   :<|> "timestamps" :> QueryFlag "bounded" :> Get '[JSON] [Timestamp]
   :<|> "tags" :> Get '[JSON] [Tag]

type API = "timeseries" :> TimeseriesApi

api :: Proxy API
api = Proxy

insertData :: [TS] -> AcidReaderT ()
insertData ts = ask >>= flip update' (InsertTS ts)
                           >>= \case
                                  [] -> return ()
                                  errors -> throwError $ err404 { errBody = C.pack $ unlines errors}

updateData :: [TS] -> AcidReaderT ()
updateData ts = ask >>= flip update' (UpdateTS ts)
                           >>= \case
                                  [] -> return ()
                                  errors -> throwError $ err404 { errBody = C.pack $ unlines errors}

deleteData :: [DTS] -> AcidReaderT ()
deleteData dts = ask >>= flip update' (ClearTS dts)
                           >>= \case
                                  [] -> return ()
                                  errors -> throwError $ err404 { errBody = C.pack $ unlines errors}

getData :: AcidReaderT [TS]
getData = queryData emptyQM >>= \(QR r) -> either return (const $ throwError err500) r

queryData :: QueryModel
           -> AcidReaderT QueryR
queryData qm  | fst $ illegalQM qm = throwError $ err404 { errBody = C.pack $ snd $ illegalQM qm }
              | otherwise = ask >>= flip query' (FilterTS qm) >>= either
                                                                  (\m -> throwError $ err404 { errBody = C.pack m })
                                                                  return

timestamps :: Bool -> AcidReaderT [Timestamp]
timestamps b = ask >>= flip query' (AllTimestamps b)
                        >>= either (\m -> throwError $ err404 { errBody = C.pack m }) return

tags :: AcidReaderT [Tag]
tags = ask >>= flip query' AllTags

tsHandlers :: TSServer TimeseriesApi
tsHandlers = insertData
        :<|> updateData
        :<|> queryData
        :<|> getData
        :<|> deleteData
        :<|> deleteData []
        :<|> timestamps
        :<|> tags

serverT :: TSServer API
serverT = tsHandlers

app :: AcidState TimeseriesDB -> Application
app db = serve api $ hoistServer api (`runReaderT` db) serverT
