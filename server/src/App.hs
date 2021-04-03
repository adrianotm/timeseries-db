module App
    ( startApp
    ) where

import           Control.Exception.Base   (bracket)

import           Api                      (app)
import           Data.Acid.Local          (createCheckpointAndClose,
                                           openLocalState)
import           Data.IntMap              as IM (empty)
import           Data.Vector              as V (empty)
import           DataS.HashMap            as HM (empty)
import           GHC.Compact              (compact)
import           Network.Wai.Handler.Warp (run)
import           Repository.Model         (DB (..), TimeseriesDB (TimeseriesDB))

startApp :: IO ()
startApp = bracket
            (compact (TimeseriesDB IM.empty HM.empty V.empty) >>= openLocalState . DB)
            createCheckpointAndClose
            (run 8081 . app)
