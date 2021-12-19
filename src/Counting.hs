{-# LANGUAGE RankNTypes #-}

module Counting
  ( Color (..),
    territories,
    territoryFor,
  )
where

import Data.Function
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set

data Color = Black | White deriving (Eq, Ord, Show)
type Coord = (Int, Int)

territories :: [String] -> [(Set Coord, Maybe Color)]
territories board = error "You need to implement this function."

territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor board coord =
  coord
    & searchForAllReachableNodes (getEmptyNeighbors board)
    & extractEmptyTerritoryAndColors board
    & tryGetTerritoryWithOwner
  where
    getMaxCoord :: [String] -> Coord
    getMaxCoord board =
      error "You need to implement this function."

    tryGetColor :: [String] -> Coord -> Maybe Color
    tryGetColor =
      error "You need to implement this function."

    searchForAllReachableNodes :: forall a. Ord a => (a -> Set a) -> a -> Set a
    searchForAllReachableNodes getSuccessors node =
      dfs getSuccessors Set.empty node
      where 
        dfs getSuccessors visitedSet node = 
          node
          & getSuccessors 
          & Set.foldl' dfsIfNotVisited visitedSetExpanded
          where 
            dfsIfNotVisited visited node =
              if not $ Set.member node visited 
                then dfs getSuccessors visited node
                else visited
            visitedSetExpanded = Set.insert node visitedSet

    getEmptyNeighbors :: [String] -> Coord -> Set Coord
    getEmptyNeighbors board coord =
      case tryGetColor board coord of
        Just _ -> Set.empty
        Nothing ->
          getNeighbors board coord
          & Set.filter (Maybe.isNothing . tryGetColor board)
      where
        getNeighbors board (row, col) =
          [(row - 1, col) | row > 1] 
          ++ [(row + 1, col) | row < maxRow] 
          ++ [(row, col - 1) | col > 1] 
          ++ [(row, col + 1) | col < maxCol]
          & Set.fromList
          where (maxRow, maxCol) = getMaxCoord board

    extractEmptyTerritoryAndColors :: [String] -> Set Coord -> (Set Coord, Set Color)
    extractEmptyTerritoryAndColors board territory =
      let
        (emptyCoords, occupiedCoords) =
          territory
          & Set.partition (Maybe.isNothing . tryGetColor board)
        owners =
          occupiedCoords
          & Set.toList
          & Maybe.mapMaybe (tryGetColor board)
          & Set.fromList
      in (emptyCoords, owners)

    tryGetTerritoryWithOwner :: (Set Coord, Set Color) -> Maybe (Set Coord, Maybe Color)
    tryGetTerritoryWithOwner (coords, colors)
      | Set.null coords = Nothing
      | otherwise = Just (coords, tryGetSingleton colors)
      where
        tryGetSingleton set
          | Set.size set == 1 = Just $ Set.elemAt 0 set
          | otherwise = Nothing