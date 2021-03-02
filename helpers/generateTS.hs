import           Data.Aeson.Text
import           Data.Text.Lazy.IO     as I
import           Data.Time.Calendar
import           Data.Time.Clock.POSIX
import           Repository.Model

generateTS :: IO [TS]
generateTS = do now <- getPOSIXTime
                return [TS (round (1111 + fromIntegral i)) (Tag $ Left "adrian") 66 | i <- [0..100]]

demoTSFile :: IO ()
demoTSFile = do ts <- generateTS
                I.writeFile "demoTS.json" (encodeToLazyText ts)
