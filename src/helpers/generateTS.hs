import           Data.Aeson.Text
import           Data.Text.Lazy.IO     as I
import           Data.Time.Calendar
import           Data.Time.Clock.POSIX
import           Repository.Model

getTag :: Int -> Tag
getTag i
  | even i = Tag $ Left "drop"
  | i `mod` 3 == 0  = Tag $ Left "adrian"
  | i `mod` 5 == 0  = Tag $ Left "cholak"
  | otherwise  = Tag $ Left "test"

getTs :: Int -> Int
getTs i
  | even i = 1
  | i `mod` 3 == 0  = 2
  | i `mod` 5 == 0  = 3
  | otherwise  = 4

generateTS :: IO [TS]
generateTS = do now <- getPOSIXTime
                return [TS (i `mod` 10) (Tag $ Right (0 + fromIntegral i)) 66 | i <- [0..1000000]]

demoTSFile :: IO ()
demoTSFile = do ts <- generateTS
                I.writeFile "demoTS.json" (encodeToLazyText ts)
