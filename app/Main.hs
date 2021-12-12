module Main where

import LeapYear
import Pangram
import Data.Function

main :: IO ()
main = do
    print $ isLeapYear 2020
    print $ isPangram ""
    print $ 8.9 & max 6.7 & replicate 5 & sum