module CreateICS
( EventICS (..)
, createICS
, durationHrs
, durationMins
) where

import Data.Fixed
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import System.IO
import Text.Printf

data EventICS = EventICS { summary :: [Char], time :: LocalTime, duration :: NominalDiffTime } deriving (Show)

createICS :: [Char] -> [EventICS] -> IO ()
createICS filename events = do
    handle <- openFile filename WriteMode
    hPutStr handle calendarHead
    currentTime <- getZonedTime
    hPutStr handle $ concat $ map (printEvent currentTime) events
    hPutStr handle calendarTail
    hClose handle

durationHrs :: Pico -> Pico
durationHrs = (60*60 *)

durationMins :: Pico -> Pico
durationMins = (60 *)

calendarHead = "BEGIN:VCALENDAR\n"
            ++ "VERSION:2.0\n"
            ++ "PRODID:-//ptrchas3//NONSGML v1.0//EN\n"

calendarTail = "END:VCALENDAR"

eventHead :: [Char]
eventHead = "BEGIN:VEVENT\n"
         ++ "UID:uid1@example.com\n"
         ++ "ORGANIZER;CN=Peter Chase:MAILTO:peter.chase@ptrchas3.com\n"

eventTail :: [Char]
eventTail = "END:VEVENT\n"

printTime :: LocalTime -> [Char]
printTime time = printf "%04d%02d%02dT%02d%02d%02d" yyyy mm dd hour min sec
    where (yyyy, mm, dd) = toGregorian $ localDay $ time
          tod = localTimeOfDay $ time
          hour = todHour tod
          min = todMin tod
          sec = (round $ todSec tod) :: Int

printEvent :: ZonedTime -> EventICS -> [Char]
printEvent currentTime event =
    eventHead
    -- TODO: need to create TZID from currentTime time zone
    ++ "DTSTAMP;TZID=America/Chicago:" ++ (printTime $ zonedTimeToLocalTime currentTime) ++ "\n" 
    ++ "DTSTART;TZID=America/Chicago:" ++ (printTime $ time event) ++ "\n"
    ++ "DURATION:PT" ++ (showFixed True $ nominalDiffTimeToSeconds $ duration event) ++ "S\n"
    ++ "SUMMARY:" ++ (summary event) ++ "\n"
    ++ eventTail

happyHour = EventICS summary time duration
    where summary = "Happy Hour"
          time = LocalTime (fromGregorian 2021 09 19) (TimeOfDay 17 00 00)
          duration = secondsToNominalDiffTime $ durationHrs 2

main = do
    handle <- openFile "test.ics" WriteMode
    hPutStr handle calendarHead
    currentTime <- getZonedTime
    hPutStr handle $ printEvent currentTime happyHour
    hPutStr handle calendarTail
    hClose handle
