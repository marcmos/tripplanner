{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Fetch where

import Lib

import Data.Text (Text)
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)

data TtssTrip =
  TtssTrip { actualRelativeTime :: Int
           , direction :: !Text
           , patternText :: !Text
  } deriving (Show, Generic)

data TtssStop =
  TtssStop { actual :: [TtssTrip]
           , stopName :: !Text
  } deriving (Show, Generic)

instance FromJSON TtssTrip
instance FromJSON TtssStop

jsonUrl = "https://mpk.jacekk.net/proxy_tram.php/services/passageInfo/stopPassages/stop?stop=2811&mode=departure"

stopMovement :: Text -> Text -> Int -> TtssTrip -> Movement
stopMovement firstStop lastStop tripTime ttssTrip =
  Movement {
    movementTrip = Trip (patternText ttssTrip) (direction ttssTrip)
  , movementStartName = firstStop
  , movementStopName = lastStop
  , movementWaitTime = actualRelativeTime ttssTrip
  , movementTripTime = tripTime
  }

fetch :: String -> IO (Maybe TtssStop)
fetch url = decode <$> simpleHttp url

fetchMuzeum = fetch "https://mpk.jacekk.net/proxy_tram.php/services/passageInfo/stopPassages/stopPoint?stopPoint=281129&mode=departure"
fetchMogilskie = fetch "https://mpk.jacekk.net/proxy_tram.php/services/passageInfo/stopPassages/stopPoint?stopPoint=12539&mode=departure"
fetchGrzegorzeckie = fetch "https://mpk.jacekk.net/proxy_tram.php/services/passageInfo/stopPassages/stopPoint?stopPoint=36519&mode=departure"
fetchGrzegorzeckieBus = fetch "https://mpk.jacekk.net/proxy_bus.php/services/passageInfo/stopPassages/stopPoint?stopPoint=36501&mode=departure"
fetchWieczysta = fetch "https://mpk.jacekk.net/proxy_bus.php/services/passageInfo/stopPassages/stopPoint?stopPoint=304004&mode=departure"

wieczystaBus muzTrips wieTrips = joinMovements wieMovements (map return muzMovements)
  where muzMovements = map (stopMovement "Muzeum Lotnictwa" "Wieczysta" $ 2 * 60) . actual $ muzTrips
        wieMovements = map (stopMovement "Wieczysta" "Dąbie" $ 5 * 60) . actual $ wieTrips

mogil14 muzTrips mogTrips = joinMovements mogil14Movements (map return muzMovements)
  where muzMovements = map (stopMovement "Muzeum Lotnictwa" "Mogilskie" $ 9 * 60) . actual $ muzTrips
        mogil14Movements = map (stopMovement "Mogilskie" "Dąbie" $ 8 * 60) . filter (\x -> patternText x == "14") . actual $ mogTrips

grzeg9 muzTrips grzegTrips grzegBusTrips = joinMovements grzegMovements $ (map return muz9Movements) ++ (map return grzegBusMovements)
  where grzegMovements = map (stopMovement "Grzegórzeckie" "Dąbie" $ 5 * 60) . actual $ grzegTrips
        grzegBusMovements = map (stopMovement "Grzegórzeckie" "Dąbie" $ 5 * 60) . actual $ grzegBusTrips
        muz9Movements = map (stopMovement "Muzeum Lotnictwa" "Grzegórzeckie" $ 12 * 60) . filter (\x -> patternText x == "9" || patternText x == "49") . actual $ muzTrips

mogGrzeg muzTrips mogTrips grzegTrips grzegBusTrips = joinMovements grzegMov (joinMovements mogMov (map return muzMov))
  where grzegMov =
          (map (stopMovement "Grzegórzeckie" "Dąbie" $ 5 * 60) . actual $ grzegTrips) ++
          (map (stopMovement "Grzegórzeckie" "Dąbie" $ 5 * 60) . actual $ grzegBusTrips)
        mogMov = map (stopMovement "Mogilskie" "Grzegórzeckie" $ 3 * 60) . actual $ mogTrips
        muzMov = map (stopMovement "Muzeum Lotnictwa" "Mogilskie" $ 9 * 60) . actual $ muzTrips

fetchTrips :: IO (Either String TtssStop)
fetchTrips =
    eitherDecode <$> simpleHttp jsonUrl

xdd = do
  w <- fetchWieczysta
  m <- fetchMogilskie
  g <- fetchGrzegorzeckie
  gb <- fetchGrzegorzeckieBus
  muz <- fetchMuzeum
  return (w, m, g, gb, muz)

allTrips = do
  (Just w, Just m, Just g, Just gb, Just muz) <- xdd
  return $ wieczystaBus muz w ++ grzeg9 muz g gb ++ mogil14 muz w ++ mogGrzeg muz m g gb

(Just wieczysta, Just mogilskie, Just grzegorzeckie, Just gBus, Just muzeum) = (Just (TtssStop {actual = [TtssTrip {actualRelativeTime = 1512, direction = "Zajezdnia P\322asz\243w", patternText = "128"}], stopName = "TAURON Arena Krak\243w Wieczysta"}),Just (TtssStop {actual = [TtssTrip {actualRelativeTime = 12, direction = "Mistrzejowice", patternText = "14"},TtssTrip {actualRelativeTime = 132, direction = "Ma\322y P\322asz\243w P+R", patternText = "20"},TtssTrip {actualRelativeTime = 252, direction = "Kurdwan\243w P+R", patternText = "50"},TtssTrip {actualRelativeTime = 552, direction = "Ma\322y P\322asz\243w P+R", patternText = "20"},TtssTrip {actualRelativeTime = 612, direction = "Borek Fa\322\281cki", patternText = "19"},TtssTrip {actualRelativeTime = 732, direction = "Kurdwan\243w P+R", patternText = "50"},TtssTrip {actualRelativeTime = 852, direction = "Nowy Bie\380an\243w P+R", patternText = "9"},TtssTrip {actualRelativeTime = 912, direction = "Mistrzejowice", patternText = "14"},TtssTrip {actualRelativeTime = 1032, direction = "Ma\322y P\322asz\243w P+R", patternText = "20"},TtssTrip {actualRelativeTime = 1152, direction = "Kurdwan\243w P+R", patternText = "50"},TtssTrip {actualRelativeTime = 1452, direction = "Ma\322y P\322asz\243w P+R", patternText = "20"},TtssTrip {actualRelativeTime = 1512, direction = "Borek Fa\322\281cki", patternText = "19"}], stopName = "Rondo Mogilskie"}),Just (TtssStop {actual = [TtssTrip {actualRelativeTime = 71, direction = "Wzg\243rza K.", patternText = "71"},TtssTrip {actualRelativeTime = 191, direction = "Mistrzejowice", patternText = "14"},TtssTrip {actualRelativeTime = 551, direction = "Wzg\243rza K.", patternText = "1"},TtssTrip {actualRelativeTime = 791, direction = "Kopiec Wandy", patternText = "22"},TtssTrip {actualRelativeTime = 971, direction = "Wzg\243rza K.", patternText = "71"},TtssTrip {actualRelativeTime = 1091, direction = "Mistrzejowice", patternText = "14"},TtssTrip {actualRelativeTime = 1391, direction = "Wzg\243rza K.", patternText = "1"}], stopName = "Rondo Grzeg\243rzeckie"}),Just (TtssStop {actual = [TtssTrip {actualRelativeTime = 1031, direction = "Z\322ocie\324", patternText = "125"}], stopName = "Rondo Grzeg\243rzeckie"}),Just (TtssStop {actual = [TtssTrip {actualRelativeTime = 11, direction = "Salwator", patternText = "70"},TtssTrip {actualRelativeTime = 131, direction = "Czerwone Maki P+R", patternText = "52"},TtssTrip {actualRelativeTime = 311, direction = "Nowy Bie\380an\243w P+R", patternText = "9"},TtssTrip {actualRelativeTime = 491, direction = "Bronowice Ma\322e", patternText = "4"},TtssTrip {actualRelativeTime = 551, direction = "Czerwone Maki P+R", patternText = "52"},TtssTrip {actualRelativeTime = 671, direction = "Krowodrza G\243rka", patternText = "5"},TtssTrip {actualRelativeTime = 851, direction = "Salwator", patternText = "70"},TtssTrip {actualRelativeTime = 1031, direction = "Czerwone Maki P+R", patternText = "52"},TtssTrip {actualRelativeTime = 1211, direction = "Nowy Bie\380an\243w P+R", patternText = "9"},TtssTrip {actualRelativeTime = 1451, direction = "Czerwone Maki P+R", patternText = "52"},TtssTrip {actualRelativeTime = 1451, direction = "Bronowice Ma\322e", patternText = "4"}], stopName = "Muzeum Lotnictwa"}))
(Just wieczysta2, Just mogilskie2, Just grzeg2, Just gBus2, Just muz2) = (Just (TtssStop {actual = [TtssTrip {actualRelativeTime = -95, direction = "Zajezdnia P\322asz\243w", patternText = "128"}], stopName = "TAURON Arena Krak\243w Wieczysta"}),Just (TtssStop {actual = [TtssTrip {actualRelativeTime = 25, direction = "Mistrzejowice", patternText = "14"},TtssTrip {actualRelativeTime = 145, direction = "Nowy Bie\380an\243w P+R", patternText = "9"},TtssTrip {actualRelativeTime = 985, direction = "Kurdwan\243w P+R", patternText = "50"},TtssTrip {actualRelativeTime = 1045, direction = "Ma\322y P\322asz\243w P+R", patternText = "20"}], stopName = "Rondo Mogilskie"}),Just (TtssStop {actual = [TtssTrip {actualRelativeTime = 24, direction = "Kombinat", patternText = "22"},TtssTrip {actualRelativeTime = 204, direction = "Mistrzejowice", patternText = "14"},TtssTrip {actualRelativeTime = 564, direction = "Wzg\243rza K.", patternText = "1"},TtssTrip {actualRelativeTime = 1864, direction = "Kombinat", patternText = "22"}], stopName = "Rondo Grzeg\243rzeckie"}),Just (TtssStop {actual = [TtssTrip {actualRelativeTime = 1044, direction = "Z\322ocie\324", patternText = "125"}], stopName = "Rondo Grzeg\243rzeckie"}),Just (TtssStop {actual = [TtssTrip {actualRelativeTime = 144, direction = "Czerwone Maki P+R", patternText = "52"},TtssTrip {actualRelativeTime = 444, direction = "Krowodrza G\243rka", patternText = "5"},TtssTrip {actualRelativeTime = 504, direction = "Salwator", patternText = "70"},TtssTrip {actualRelativeTime = 744, direction = "Bronowice Ma\322e", patternText = "4"}], stopName = "Muzeum Lotnictwa"}))