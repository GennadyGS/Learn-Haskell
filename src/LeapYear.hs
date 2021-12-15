module LeapYear where

isLeapYear :: Integer -> Bool
isLeapYear year
  | year `mod` 4 /= 0 = False
  | year `mod` 100 /= 0 = True
  | otherwise = year `mod` 400 == 0