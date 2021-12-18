module Counting (
    Color(..),
    territories,
    territoryFor
) where

import Data.Set (Set)
import Data.Function

data Color = Black | White deriving (Eq, Ord, Show)
type Coord = (Int, Int)

territories :: [String] -> [(Set Coord, Maybe Color)]
territories board = error "You need to implement this function."

territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor board coord = 
  coord
  & traverse (getEmptyNeighbors board)
  & getTerritoryWithBorderColors
  & tryGetTerritoryWithOwner
  where
    traverse getSuccessors root =
      error "You need to implement this function."
    getEmptyNeighbors board coord = 
      error "You need to implement this function."
    getTerritoryWithBorderColors territoryWithBorders = 
      error "You need to implement this function."
    tryGetTerritoryWithOwner territoryWithBorderColors = 
      error "You need to implement this function."
