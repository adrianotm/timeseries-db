module Lib
    ( startApp
    ) where

import           Control.Exception.Base   (bracket)

import           Data.Acid.Local
import           Data.IntMap              as IM
import           Data.Vector              as V
import           Map                      as M
import           Network.Wai.Handler.Warp

import           Api
import           Repository.Model

startApp :: IO ()
startApp = bracket
            (openLocalState $ TimeseriesDB IM.empty M.empty V.empty)
            createCheckpointAndClose
            (run 8081 . app)
