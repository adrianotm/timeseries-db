{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson.Text
import           Data.Text.Lazy.IO as I
import           Repository.Model

getTag :: Int -> Tag
getTag i
  | even i = "Munich"
  | i `mod` 3 == 0 = "Skopje"
  | i `mod` 5 == 0 = "London"
  | otherwise = "Athens"

getVal :: Int -> Val
getVal i
  | i `mod` 6 == 0 = 12
  | i `mod` 4 == 0 = 21
  | i `mod` 10 == 0 = 1223
  | otherwise = 44

getTs :: Int -> Int
getTs i
  | even i = 1
  | i `mod` 3 == 0 = 1
  | i `mod` 5 == 0 = 3
  | otherwise = 1

num = 16184810930000

interval = [num..num+2000000]

generateTS :: [TS]
generateTS = [TS (0 + i) (getTag i) (getVal i) | i <- interval]

generateDTS :: [TS']
generateDTS = [TS' (0 + i) (getTag i) | i <- interval]

demoTSFile :: IO ()
demoTSFile = I.writeFile "demoTS.json" (encodeToLazyText generateTS)

demoDTSFile :: IO ()
demoDTSFile = I.writeFile "demoTSD.json" (encodeToLazyText generateDTS)
