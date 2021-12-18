{-# LANGUAGE RankNTypes #-}

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
  & searchForAllReachableNodes (getEmptyNeighbors board)
  & extractEmptyTerritoryAndColors
  & tryGetTerritoryWithOwner
  where
    searchForAllReachableNodes :: forall a. Ord a => (a -> Set a) -> a -> Set a
    searchForAllReachableNodes getSuccessors root =
      error "You need to implement this function."
    
    getEmptyNeighbors :: [String] -> Coord -> Set Coord
    getEmptyNeighbors board coord = 
      error "You need to implement this function."
    
    extractEmptyTerritoryAndColors :: Set Coord -> (Set Coord, Set Color)
    extractEmptyTerritoryAndColors territory = 
      error "You need to implement this function."
    
    tryGetTerritoryWithOwner :: (Set Coord, Set Color) -> Maybe (Set Coord, Maybe Color)
    tryGetTerritoryWithOwner (coords, colors) = 
      error "You need to implement this function."
