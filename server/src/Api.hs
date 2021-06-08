{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Api where

import           Control.Monad.Reader        (MonadReader (ask),
                                              ReaderT (runReaderT))
import           Data.Acid                   as A (AcidState)
import           Data.Acid.Advanced          as AA (query', update')
import qualified Data.ByteString.Lazy.Char8  as C
import           Network.HTTP.Types.Method   (methodDelete, methodGet,
                                              methodPost, methodPut)
import           Network.Wai                 (Middleware)
import           Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors,
                                              simpleCorsResourcePolicy)
import           Repository.Handlers
import           Repository.Model            (QueryModel, QueryR (..), TS, TS',
                                              TimeseriesDB, illegalQM)
import           Servant

type AcidReaderT = ReaderT (AcidState TimeseriesDB) Handler

type TSServer api = ServerT api AcidReaderT

type API = "timeseries" :> TimeseriesApi

type TimeseriesApi =
  ReqBody '[JSON] [TS] :> Post '[JSON] ()
    :<|> ReqBody '[JSON] [TS] :> Put '[JSON] ()
    :<|> ReqBody '[JSON] [TS'] :> Delete '[JSON] ()
    :<|> Delete '[JSON] ()
    :<|> "query" :> ReqBody '[JSON] QueryModel :> Post '[JSON] QueryR

api :: Proxy API
api = Proxy

-- | Insert new data
--   if the insert is invalid,
--   returns a list of errors i.e. data that already exists in the database
insertData :: [TS] -> AcidReaderT ()
insertData ts =
  ask >>= flip update' (InsertTS ts)
    >>= \case
      [] -> return ()
      errors -> throwError $ err400 {errBody = C.pack $ unlines errors}

-- | Update data
--   if the update is invalid,
--   returns a list of errors i.e. data that doesn't exist in the database
updateData :: [TS] -> AcidReaderT ()
updateData ts =
  ask >>= flip update' (UpdateTS ts)
    >>= \case
      [] -> return ()
      errors -> throwError $ err400 {errBody = C.pack $ unlines errors}

-- | Delete data
--   if the delete is invalid
--   returns a list of errors i.e. data that doesn't exist in the database
deleteData :: [TS'] -> AcidReaderT ()
deleteData dts =
  ask >>= flip update' (ClearTS dts)
    >>= \case
      [] -> return ()
      errors -> throwError $ err400 {errBody = C.pack $ unlines errors}

-- | Query data
--   returns either an error or the query result
queryData ::
  QueryModel ->
  AcidReaderT QueryR
queryData qm
  | fst $ illegalQM qm = throwError $ err400 {errBody = C.pack $ snd $ illegalQM qm}
  | otherwise =
    ask >>= flip query' (FilterTS qm)
      >>= either
        (\m -> throwError $ err400 {errBody = C.pack m})
        return

serverT :: TSServer API
serverT =
  insertData
    :<|> updateData
    :<|> deleteData
    :<|> deleteData []
    :<|> queryData


corsPolicy :: Middleware
corsPolicy = cors (const $ Just policy)
  where
    policy =
      simpleCorsResourcePolicy
        { corsMethods = [methodGet, methodPost, methodPut, methodDelete],
          corsOrigins = Nothing,
          corsRequestHeaders = ["Content-Type"]
        }

app :: AcidState TimeseriesDB -> Application
app db = corsPolicy $ serve api $ hoistServer api (`runReaderT` db) serverT
