module Pangram (isPangram) where

import qualified Data.Char as Char
import Data.Function
import qualified Data.List as List

isPangram :: String -> Bool
isPangram text =
  let textInLowerCase = text & List.map Char.toLower
    in ['a' .. 'z']
    & List.all (`elem` textInLowerCase)
