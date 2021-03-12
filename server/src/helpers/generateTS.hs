import           Data.Aeson.Text
import qualified Data.DList            as DL
import           Data.Foldable
import           Data.Semigroup
import           Data.Text.Lazy.IO     as I
import           Data.Time.Calendar
import           Data.Time.Clock.POSIX
import           Model

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

generateTS :: [TS]
generateTS = [TS (0 + i) (Tag $ Left "adrian") 66 | i <- [0..1500000]]

demoTSFile :: IO ()
demoTSFile = I.writeFile "demoTS3.json" (encodeToLazyText generateTS)
