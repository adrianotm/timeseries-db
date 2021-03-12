{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module Main where

import           Data.Proxy
import           Data.Text
import           GHC.Generics
import           Miso
import           Miso.String
import           Network.HTTP.Client (defaultManagerSettings, newManager)
import           Servant.API
import           Servant.Client

import           Api
import           Control.Lens
import           Model

uploadData  :<|> updateData :<|> marketing :<|> queryData :<|> getAll :<|> deleteData  :<|> deleteAll :<|> allTS :<|> allTags = client api

newtype Model = Model { _ts :: [TS] }

makeLenses ''TS
makeLenses ''Model

data Action
  = UploadData
  | SayHelloWorld
  deriving (Show, Eq)

main :: IO ()
main = startApp App {..}
  where
    initialAction = SayHelloWorld -- initial action to be executed on application load
    model  = Model [TS 10 (Left "Yes") 50]              -- initial model
    update = fromTransition . updateModel -- update function
    view   = viewModel            -- view function
    events = defaultEvents        -- default delegated events
    subs   = []                   -- empty subscription list
    mountPoint = Nothing          -- mount point for application (Nothing defaults to 'body')
    logLevel = Off                -- used during prerendering to see if the VDOM and DOM are in sync (only applies to `miso` function)

viewModel :: Model -> View Action
viewModel m = div_ [] [
   input_
   [ type_ "file"
   , multiple_ False
   , onChange (const AddOne)
   ]
 , table_
   [ tr_ [ th_ "Timestamp"
         , th_ "Tag"
         , th_ "Value"
         ]
   , Prelude.map viewTS x
   ]
 ]

viewTS :: TS -> View Action
viewTS ts
  = tr_ [ td_ ts^.timestamp
        , td_ ts^.tag
        , td_ ts^.value
        ]

updateModel :: Action -> Transition Action Model ()
updateModel action =
  case action of
    SayHelloWorld
      -> scheduleIO_ (consoleLog "Hello World")
