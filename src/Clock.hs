module Clock (addDelta, fromHourMin, toString) where

import Text.Printf

data Clock = Clock
  { hour :: Int,
    minute :: Int
  }
  deriving (Eq)

hoursPerDay = 24
minutesPerHour = 60

fromHourMin :: Int -> Int -> Clock
fromHourMin hour minute = 
  let 
    minutesPerDay = hoursPerDay * minutesPerHour
    totalMinutes = (hour * minutesPerHour + minute) `mod` minutesPerDay
  in 
    Clock {
      hour = totalMinutes `div` minutesPerHour, 
      minute = totalMinutes `mod` minutesPerHour
    }

toString :: Clock -> String
toString clock = 
  printf "%02d:%02d" (hour clock) (minute clock)

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour minutes clock = 
  fromHourMin 
    (Clock.hour clock + hour) 
    (Clock.minute clock + minutes)
