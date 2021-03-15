{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
module Api where

import           Control.Exception.Base      (bracket)

import           Control.Monad.Reader        (MonadReader (ask),
                                              ReaderT (runReaderT))
import           Data.Acid                   as A (AcidState)
import           Data.Acid.Advanced          as AA (query', update')
import           Data.Aeson                  (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy.Char8  as C
import           Network.HTTP.Types.Method   (methodDelete, methodGet,
                                              methodPost, methodPut)
import           Network.Wai                 (Application, Middleware)
import           Network.Wai.Middleware.Cors (CorsResourcePolicy (..),
                                              CorsResourcePolicy, cors,
                                              simpleCorsResourcePolicy)
import           Repository.Handlers
import           Repository.Model            (DTS, QueryModel, QueryR (..), TS,
                                              Tag, TimeseriesDB, Timestamp)
import           Repository.Utils            (illegalQM)
import           Servant

type AcidReaderT = ReaderT (AcidState TimeseriesDB) Handler
type TSServer api = ServerT api AcidReaderT

type TimeseriesApi =
   ReqBody '[JSON] [TS] :> Post '[JSON] ()
   :<|> ReqBody '[JSON] [TS] :> Put '[JSON] ()
   :<|> ReqBody '[JSON] [DTS] :> Delete '[JSON] ()
   :<|> Delete '[JSON] ()
   :<|> "query" :> ReqBody '[JSON] QueryModel :> Post '[JSON] QueryR
   :<|> "timestamps" :> QueryFlag "bounded" :> Get '[JSON] [Timestamp]
   :<|> "tags" :> Get '[JSON] [Tag]

type API = "timeseries" :> TimeseriesApi

api :: Proxy API
api = Proxy

insertData :: [TS] -> AcidReaderT ()
insertData ts = ask >>= flip update' (InsertTS ts)
                           >>= \case
                                  [] -> return ()
                                  errors -> throwError $ err400 { errBody = C.pack $ unlines errors}

updateData :: [TS] -> AcidReaderT ()
updateData ts = ask >>= flip update' (UpdateTS ts)
                           >>= \case
                                  [] -> return ()
                                  errors -> throwError $ err400 { errBody = C.pack $ unlines errors}

deleteData :: [DTS] -> AcidReaderT ()
deleteData dts = ask >>= flip update' (ClearTS dts)
                           >>= \case
                                  [] -> return ()
                                  errors -> throwError $ err400 { errBody = C.pack $ unlines errors}

queryData :: QueryModel
           -> AcidReaderT QueryR
queryData qm  | fst $ illegalQM qm = throwError $ err400 { errBody = C.pack $ snd $ illegalQM qm }
              | otherwise = ask >>= flip query' (FilterTS qm) >>= either
                                                                  (\m -> throwError $ err400 { errBody = C.pack m })
                                                                  return

timestamps :: Bool -> AcidReaderT [Timestamp]
timestamps b = ask >>= flip query' (AllTimestamps b)
                        >>= either (\m -> throwError $ err400 { errBody = C.pack m }) return

tags :: AcidReaderT [Tag]
tags = ask >>= flip query' AllTags

tsHandlers :: TSServer TimeseriesApi
tsHandlers = insertData
        :<|> updateData
        :<|> deleteData
        :<|> deleteData []
        :<|> queryData
        :<|> timestamps
        :<|> tags

serverT :: TSServer API
serverT = tsHandlers

corsPolicy :: Middleware
corsPolicy = cors (const $ Just policy)
    where
        policy = simpleCorsResourcePolicy
          {
              corsMethods = [methodGet, methodPost, methodPut, methodDelete],
              corsOrigins = Just (["http://localhost:8000"], False),
              corsRequestHeaders = [ "Content-Type" ]
          }

app :: AcidState TimeseriesDB -> Application
app db = corsPolicy $ serve api $ hoistServer api (`runReaderT` db) serverT
