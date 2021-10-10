module Main where

import CreateICS
import SolarNoon

import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime

dallas = Location (-6) $ LL (32.78) (-96.80)

daysOfYear :: Integer -> [Day]
daysOfYear year = [a..b]
    where a = fromGregorian year 01 01
          b = fromGregorian year 12 31

noon :: Day -> EventICS
noon day = EventICS summary time duration
           where summary = "Solar Noon"
                 time = LocalTime day $ solarNoon' dallas day
                 duration = secondsToNominalDiffTime $ durationMins 1

noons :: [Day] -> [EventICS]
noons days =
    map noon days

sleep :: Day -> EventICS
sleep day = EventICS summary time duration
           where summary = "Sleep"
                 time = LocalTime day $ TimeOfDay (8 + todHour sn) (todMin sn) (todSec sn)
                 duration = secondsToNominalDiffTime $ durationHrs 8
                 sn = solarNoon' dallas day

sleeps :: [Day] -> [EventICS]
sleeps days =
    map sleep days

events :: Integer -> [EventICS]
events year = 
    (noons  $ daysOfYear year) ++
    (sleeps $ daysOfYear year)

main = do
    createICS "SolarSchedule2021.ics" $ events 2021
