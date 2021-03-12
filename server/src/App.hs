module App
    ( startApp
    ) where

import           Control.Exception.Base   (bracket)

import           Data.Acid.Local
import           Data.IntMap              as IM
import           Data.Vector              as V
import           DataS.HashMap            as HM
import           Network.Wai.Handler.Warp

import           ApiHandlers
import           Model
import           Repository.Model

startApp :: IO ()
startApp = bracket
            (openLocalState $ TimeseriesDB IM.empty HM.empty V.empty)
            createCheckpointAndClose
            (run 8081 . app)
