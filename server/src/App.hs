module App
    ( startApp
    ) where

import           Control.Exception.Base   (bracket)

import           Data.Acid.Local          (createCheckpointAndClose,
                                           openLocalState)
import           Data.IntMap              as IM (empty)
import           Data.Vector              as V (empty)
import           DataS.HashMap            as HM (empty)
import           Network.Wai.Handler.Warp (run)

import           Api                      (app)
import           Repository.Model         (TimeseriesDB (TimeseriesDB))

startApp :: IO ()
startApp = bracket
            (openLocalState $ TimeseriesDB IM.empty HM.empty V.empty)
            createCheckpointAndClose
            (run 8081 . app)
