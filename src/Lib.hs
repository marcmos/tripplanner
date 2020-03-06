{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.Text (Text, pack)
import qualified Data.Text as T (concat)
import Data.Maybe (mapMaybe)
import Data.List (intersperse)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- data WaitTime = ScheduleWait Int | ActualWait Int
-- waitTime :: Stop -> LineNum -> (MyTrip, WaitTime)
-- waitTimeAny :: Stop -> (MyTrip, WaitTime)
--
-- data Trip = Trip { line :: Int, Headsign :: String }
-- data Movement = Movement {
--	trip :: Trip,
--	start, stop :: String,
--	waitTime, tripTime :: Int }
-- movementTime :: Stop -> Stop -> Movement
--
-- movementSeqTime :: [Movement] -> Int
-- joinMovement :: [Movement] -> Movement -> Maybe [Movement]
-- stopMovements -> Stop -> [Movement]

data Trip = Trip {
    lineTrip :: Text
  , lineHeadsign :: Text
} deriving Show

data Movement = Movement {
    movementTrip :: Trip
  , movementStartName :: Text
  , movementStopName  :: Text
  , movementWaitTime :: Int
  , movementTripTime :: Int
} deriving Show

movementSeqTime :: [Movement] -> Int
movementSeqTime = sum . map (\m -> movementTripTime m + movementWaitTime m)

relativeWaitToMovement :: [Movement] -> Movement -> Movement
relativeWaitToMovement prevMov nextMov =
    nextMov { movementWaitTime = movementWaitTime nextMov - sumTimeToNextMov }
    where sumTimeToNextMov = movementSeqTime prevMov

joinMovement :: [Movement] -> Movement -> Maybe [Movement]
joinMovement x m
  | movementWaitTime m >= movementSeqTime x =
      Just $ relativeWaitToMovement x m : x
  | otherwise = Nothing


joinMovements :: [Movement] -> [[Movement]] -> [[Movement]]
joinMovements stopM tripM = tripM >>= (\trip -> mapMaybe (joinMovement trip) stopM)

t14M = Trip "14" "Mistrzejowice"

awf52 = Movement (Trip "52" "Maki") "AWF" "Mogilskie" 5 9
awfTest = Movement (Trip "9" "Borek Fałęcki") "AWF" "Grzegórzeckie" 2 10

movementStr :: Movement -> Text
movementStr m = T.concat ["+", pack $ show $ movementWaitTime m `div` 60, "m ", movementStartName m, " (", lineTrip . movementTrip $ m, ")"]

movementSeqStr :: [Movement] -> Text
movementSeqStr x = T.concat [tripStr, " | ", etaStr]
  where tripStr = T.concat $ intersperse " -> " (map movementStr (reverse x))
        etaStr = T.concat ["ETA: ", pack . show . (`div` 60) . movementSeqTime $ x, "m"]

formatTrip :: [[Movement]] -> [Text]
formatTrip = map movementSeqStr

mogil50borek = Movement (Trip "50" "Borek F.") "Mogilskie" "Grzegórzeckie" 13 2
-- dabie22 = Movement (Trip 22 "Wzg. K.") "Grzegórzeckie" "Dąbie" 25 5
mogilskie14 = Movement t14M "Mogilskie" "Dąbie" 20 1
-- mogilskie14Test = Movement t14M "Grzegórzeckie" "Dąbie" 15 15

muzMovRl = [Movement {movementTrip = Trip {lineTrip = "52", lineHeadsign = "Czerwone Maki P+R"}, movementStartName = "Muzeum Lotnictwa", movementStopName = "Rondo Mogilskie", movementWaitTime = 264, movementTripTime = 540},Movement {movementTrip = Trip {lineTrip = "5", lineHeadsign = "Krowodrza G\243rka"}, movementStartName = "Muzeum Lotnictwa", movementStopName = "Rondo Mogilskie", movementWaitTime = 564, movementTripTime = 540},Movement {movementTrip = Trip {lineTrip = "70", lineHeadsign = "Salwator"}, movementStartName = "Muzeum Lotnictwa", movementStopName = "Rondo Mogilskie", movementWaitTime = 744, movementTripTime = 540},Movement {movementTrip = Trip {lineTrip = "4", lineHeadsign = "Bronowice Ma\322e"}, movementStartName = "Muzeum Lotnictwa", movementStopName = "Rondo Mogilskie", movementWaitTime = 924, movementTripTime = 540},Movement {movementTrip = Trip {lineTrip = "9", lineHeadsign = "Nowy Bie\380an\243w P+R"}, movementStartName = "Muzeum Lotnictwa", movementStopName = "Rondo Mogilskie", movementWaitTime = 1104, movementTripTime = 540}]
mogMovRl = [Movement {movementTrip = Trip {lineTrip = "9", lineHeadsign = "Nowy Bie\380an\243w P+R"}, movementStartName = "Rondo Mogilskie", movementStopName = "Rondo Grzeg\243rzeckie", movementWaitTime = -60, movementTripTime = 120},Movement {movementTrip = Trip {lineTrip = "14", lineHeadsign = "Mistrzejowice"}, movementStartName = "Rondo Mogilskie", movementStopName = "Rondo Grzeg\243rzeckie", movementWaitTime = 120, movementTripTime = 120},Movement {movementTrip = Trip {lineTrip = "19", lineHeadsign = "Borek Fa\322\281cki"}, movementStartName = "Rondo Mogilskie", movementStopName = "Rondo Grzeg\243rzeckie", movementWaitTime = 720, movementTripTime = 120},Movement {movementTrip = Trip {lineTrip = "20", lineHeadsign = "Ma\322y P\322asz\243w P+R"}, movementStartName = "Rondo Mogilskie", movementStopName = "Rondo Grzeg\243rzeckie", movementWaitTime = 720, movementTripTime = 120},Movement {movementTrip = Trip {lineTrip = "50", lineHeadsign = "Kurdwan\243w P+R"}, movementStartName = "Rondo Mogilskie", movementStopName = "Rondo Grzeg\243rzeckie", movementWaitTime = 1080, movementTripTime = 120},Movement {movementTrip = Trip {lineTrip = "14", lineHeadsign = "Mistrzejowice"}, movementStartName = "Rondo Mogilskie", movementStopName = "Rondo Grzeg\243rzeckie", movementWaitTime = 1320, movementTripTime = 120}]
