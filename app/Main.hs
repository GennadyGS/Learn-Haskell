module Main where

import Data.Function
import qualified Data.List as List
import qualified Data.List.NonEmpty as List.NonEmpty
import LeapYear
import Pangram
import Control.Monad

main :: IO ()
main = do
  isLeapYear 2020 & print
  isPangram "" & print
  8.9
    & max 6.7
    & replicate 5
    & sum
    & print
  let dic = [(1, "aa"), (1, "cc"), (2, "aa"), (3, "ff"), (3, "gg"), (1, "bb")]
  dic
    & List.NonEmpty.groupAllWith fst
    & List.map (\l -> (List.NonEmpty.head l & fst, l & List.NonEmpty.toList & List.map snd))
    & print
  (-2) `mod` 6 & print
  (read "100" :: Int) & print
