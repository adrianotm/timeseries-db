import           Data.Aeson.Text
import           Data.Text.Lazy.IO     as I
import           Data.Time.Calendar
import           Data.Time.Clock.POSIX
import           Model

-- AlphaVantageToken = NRTAFC4GZBSIF787

generateTS :: IO [TS]
generateTS = do now <- getPOSIXTime
                return [TS (round (now + fromIntegral i)) (Left "abc") (Right 20) | i <- [0..60]]

demoTSFile :: IO ()
demoTSFile = do ts <- generateTS
                I.writeFile "demoTS.json" (encodeToLazyText ts)

