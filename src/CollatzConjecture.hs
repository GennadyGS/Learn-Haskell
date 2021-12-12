module CollatzConjecture (collatz) where

import Data.Function
import qualified Data.List as List

collatz :: Integer -> Maybe Integer
collatz input =
  if input <= 0
    then Nothing
    else Just $ collatzForNatural input
  where
    getNext x =
      if even x
        then x `div` 2
        else x * 3 + 1
    collatzForNatural input =
      input
        & List.iterate getNext
        & List.takeWhile (> 1)
        & List.length
        & toInteger
