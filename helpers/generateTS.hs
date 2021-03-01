import           Data.Aeson.Text
import           Data.Text.Lazy.IO     as I
import           Data.Time.Calendar
import           Data.Time.Clock.POSIX
import           Repository.Model

-- AlphaVantageToken = NRTAFC4GZBSIF787

generateTS :: IO [TS]
generateTS = do now <- getPOSIXTime
                return [TS (round (1614019694 + fromIntegral i)) (Left "adrian") 66 | i <- [0..10000]]

demoTSFile :: IO ()
demoTSFile = do ts <- generateTS
                I.writeFile "demoTS.json" (encodeToLazyText ts)