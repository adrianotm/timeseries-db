import           Data.Aeson.Text
import           Data.Text.Lazy.IO     as I
import           Data.Time.Calendar
import           Data.Time.Clock.POSIX
import           Repository.Model

getTag :: Int -> Tag
getTag i = if even i then Tag $ Left "drop"
                     else Tag $ Left "adrian"

generateTS :: IO [TS]
generateTS = do now <- getPOSIXTime
                return [TS (round (0 + fromIntegral i)) (getTag i) 66 | i <- [0..1000]]

demoTSFile :: IO ()
demoTSFile = do ts <- generateTS
                I.writeFile "demoTS.json" (encodeToLazyText ts)
