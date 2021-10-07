module SolarNoon
( LL (..)
, Location (..) 
, solarNoon
, solarNoon'
) where

import Data.Fixed -- for mod'
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime

solarNoon :: Location -> Day -> TimeOfDay 
solarNoon loc day = timeFromDouble $ (720 - 4*(lon (coord loc)) - eqOfTimeMin zt + tzOffsetMin) / 1440
    where tzOffsetMin = fromIntegral $ timeZoneMinutes $ zonedTimeZone zt
          zt = ZonedTime (LocalTime day (midday)) (hoursToTimeZone (tzOffset loc))

solarNoon' :: Location -> Day -> TimeOfDay 
solarNoon' loc day = if isDST day
    then timeToTimeOfDay $ 60*60 + timeOfDayToTime tod
    else tod
    where tod = solarNoon loc day

data LL = LL { lat :: Double, lon :: Double } deriving (Show)
data Location = Location { tzOffset :: Int, coord :: LL } deriving (Show)

washington = Location (-5) $ LL (38.88) (-77.03)
dallas = Location (-6) $ LL (32.78) (-96.80)
denver = Location (-7) $ LL (39.74) (-104.99)
losAngeles = Location (-8) $ LL (34.05) (-118.23)

degToRad :: Floating a => a -> a
degToRad a = a * pi / 180

radToDeg :: Floating a => a -> a
radToDeg a = a * 180 / pi

modJulEpoch = fromGregorian 1858 11 17
excelEpoch = fromGregorian 1899 12 30
diffEpoch = diffDays modJulEpoch excelEpoch

julianDay :: ZonedTime -> Double
julianDay zt = fromIntegral(dateValue)+2415018.5+localTime-tzOffsetMin/60/24
    where tzOffsetMin = fromIntegral $ timeZoneMinutes $ zonedTimeZone zt
          localTime = 0.5 -- noon
          dateValue = diffEpoch + (toModifiedJulianDay $ localDay $ zonedTimeToLocalTime zt)

julianCentury :: ZonedTime -> Double
julianCentury zt = (jd-2451545)/36525
    where jd = julianDay zt

eccentEarthOrbit :: Double -> Double
eccentEarthOrbit jc = 0.016708634-jc*(0.000042037+0.0000001267*jc)

geomMeanAnomSun :: Double -> Double
geomMeanAnomSun jc = degToRad $ 357.52911+jc*(35999.05029-0.0001537*jc)

geomMeanLongSun :: Double -> Double
geomMeanLongSun jc = degToRad $ mod' (280.46646+jc*(36000.76983+jc*0.0003032)) 360

meanObliqEcliptic :: Double -> Double
meanObliqEcliptic jc = 23+(26+((21.448-jc*(46.815+jc*(0.00059-jc*0.001813))))/60)/60

obliqCorr :: Double -> Double
obliqCorr jc = degToRad $ oe+0.00256*cos(degToRad(125.04-1934.136*jc))
    where oe = meanObliqEcliptic jc

variationY :: Double -> Double
variationY jc = tan(oc/2)*tan(oc/2)
    where oc = obliqCorr jc

eqOfTimeMin :: ZonedTime -> Double
eqOfTimeMin zt = 4*radToDeg ( 
    varY*sin(2*sunLon) - 2*ecc*sin(sunAnom) + 4*ecc*varY*sin(sunAnom)*cos(2*sunLon)
        - 0.5*varY*varY*sin(4*sunLon) - 1.25*ecc*ecc*sin(2*sunAnom))
    where ecc = eccentEarthOrbit jc 
          varY = variationY jc 
          sunLon = geomMeanLongSun jc 
          sunAnom = geomMeanAnomSun jc 
          jc = julianCentury zt

timeFromDouble :: Double -> TimeOfDay
timeFromDouble a = timeToTimeOfDay $ secondsToDiffTime $ round $ a * 24 * 60 * 60

isDST :: Day -> Bool
isDST day = (&&) (diffDays day (dstStart yy) >= 0) (diffDays day (dstEnd yy) < 0)
    where (yy, _, _) = toGregorian day

dstStart :: Integer -> Day
dstStart yy = addDays 7 $ firstDayOfWeekOnAfter Sunday $ fromGregorian yy 03 01

dstEnd :: Integer -> Day
dstEnd yy = firstDayOfWeekOnAfter Sunday $ fromGregorian yy 11 01

-- | @dayOfWeekDiff a b = a - b@ in range 0 to 6.
-- The number of days from b to the next a.
dayOfWeekDiff :: DayOfWeek -> DayOfWeek -> Int
dayOfWeekDiff a b = mod' (fromEnum a - fromEnum b) 7

-- | The first day-of-week on or after some day
firstDayOfWeekOnAfter :: DayOfWeek -> Day -> Day
firstDayOfWeekOnAfter dw d = addDays (toInteger $ dayOfWeekDiff dw $ dayOfWeek d) d

