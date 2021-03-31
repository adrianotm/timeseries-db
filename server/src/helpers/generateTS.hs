{-# LANGUAGE OverloadedStrings #-}
import           Data.Aeson.Text
import           Data.Text.Lazy.IO as I
import           Repository.Model

getTag :: Int -> Tag
getTag i
  | even i = "drop"
  | i `mod` 3 == 0  = "adrian"
  | i `mod` 5 == 0  = "cholak"
  | otherwise  = "test"

getVal :: Int -> Val
getVal i
  | i `mod` 6 == 0  = 12
  | i `mod` 4 == 0  = 21
  | i `mod` 10 == 0  = 1223
  | otherwise  = 44

getTs :: Int -> Int
getTs i
  | even i = 1
  | i `mod` 3 == 0  = 1
  | i `mod` 5 == 0  = 3
  | otherwise  = 1

num = 1000000

generateTS :: [TS]
generateTS = [TS (0 + i) (getTag i) (getVal i) | i <- [0..num]]

generateDTS :: [DTS]
generateDTS = [DTS (0 + i) (getTag i)| i <- [0..num]]

demoTSFile :: IO ()
demoTSFile = I.writeFile "demoTSBig.json" (encodeToLazyText generateTS)

demoDTSFile :: IO ()
demoDTSFile = I.writeFile "demoTSD.json" (encodeToLazyText generateDTS)
