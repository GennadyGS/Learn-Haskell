module List where

import Data.Function
import Data.Ord
import qualified Data.List.NonEmpty as List.NonEmpty
import qualified Data.List as List

myGroup :: (Eq a, Ord a) => [(a, b)] -> [(a, [b])]
myGroup = 
  map (\l -> (fst . head $ l, map snd l)) 
  . List.groupBy ((==) `on` fst)
  . List.sortBy (comparing fst)

groupTuples :: Ord a => [(a, b)] -> [(a, [b])]
groupTuples input =
  input
  & List.NonEmpty.groupAllWith fst
  & List.map (
      \l -> (
        List.NonEmpty.head l & fst, 
        l & List.NonEmpty.toList & List.map snd))
