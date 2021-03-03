import           Data.Aeson.Text
import           Data.Text.Lazy.IO     as I
import           Data.Time.Calendar
import           Data.Time.Clock.POSIX
import           Repository.Model

getTag :: Int -> Tag
getTag i = if even i then Tag $ Left "adrian"
                     else Tag $ Left "cholak"

generateTS :: IO [TS]
generateTS = do now <- getPOSIXTime
                return [TS (round (1111 + fromIntegral i)) (getTag i) 66 | i <- [0..300000]]

demoTSFile :: IO ()
demoTSFile = do ts <- generateTS
                I.writeFile "demoTS.json" (encodeToLazyText ts)
