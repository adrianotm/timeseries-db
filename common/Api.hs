{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
module Api where

import           Data.Proxy
import           Model
import           Servant.API

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

