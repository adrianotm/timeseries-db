module App
  ( startApp,
  )
where

import           Api                      (app)
import           Control.Exception.Base   (bracket)
import           Data.Acid.Local          (createCheckpointAndClose,
                                           openLocalCompactState,
                                           openLocalState)
import           Data.Vector              as V (empty)
import           Data.Vector.Unboxed      as UV (empty)
import           DataS.HashMap            as HM (empty)
import           DataS.IntMap             as IM (empty)
import           Network.Wai.Handler.Warp (run)
import           Repository.Model         (TimeseriesDB (TimeseriesDB))

startApp :: IO ()
startApp =
  bracket
    (openLocalState (TimeseriesDB IM.empty HM.empty))
    createCheckpointAndClose
    (run 8081 . app)
