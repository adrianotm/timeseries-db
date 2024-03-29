module App
  ( startApp,
  )
where

import           Api                      (app)
import           Control.Exception.Base   (bracket)
import           Data.Acid.Local          (createCheckpointAndClose,
                                           openLocalCompactState)
import           Data.Vector              as V (empty)
import           Data.Vector.Unboxed      as UV (empty)
import           DataS.HashMap            as HM (empty)
import           DataS.IntMap             as IM (empty)
import           Network.Wai.Handler.Warp (run)
import           Repository.Model         (TimeseriesDB (TimeseriesDB))

startApp :: IO ()
startApp =
  bracket
    (openLocalCompactState (TimeseriesDB IM.empty HM.empty V.empty UV.empty))
    createCheckpointAndClose
    (run 8081 . app)
