import           Data.Aeson.Text
import qualified Data.DList        as DL
import           Data.Text.Lazy.IO as I
import           Repository.Model

getTag :: Int -> Tag
getTag i
  | even i = "drop"
  | i `mod` 3 == 0  = "adrian"
  | i `mod` 5 == 0  = "cholak"
  | otherwise  = "test"

getTs :: Int -> Int
getTs i
  | even i = 1
  | i `mod` 3 == 0  = 2
  | i `mod` 5 == 0  = 3
  | otherwise  = 4

generateTS :: [TS]
generateTS = [TS (0 + i) "adrian" 66 | i <- [0..10]]

demoTSFile :: IO ()
demoTSFile = I.writeFile "demoTS3.json" (encodeToLazyText generateTS)
