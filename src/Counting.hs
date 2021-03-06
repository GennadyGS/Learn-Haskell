{-# LANGUAGE RankNTypes #-}

module Counting
  ( Color (..),
    territories,
    territoryFor,
  )
where

import Data.Function
import Data.List ((\\))
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Printf

data Color = Black | White deriving (Eq, Ord, Show)

type Coord = (Int, Int)

territories :: [String] -> [(Set Coord, Maybe Color)]
territories board =
  board
    & getAllCoords
    & List.filter (Maybe.isNothing . tryGetColor board)
    & splitIntoEquivalenceClasses (territoryFor board) (Set.toList . fst)

territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor board coord@(x, y)
  | x <= 0 || x > maxX || y <= 0 || y > maxY = Nothing
  | otherwise =
    coord
      & searchForAllReachableNodes (getEmptyNeighbors board)
      & extractEmptyTerritoryAndColors board
      & tryGetTerritoryWithOwner
  where
    (maxX, maxY) = getMaxCoord board

splitIntoEquivalenceClasses :: forall a b. Eq a => (a -> Maybe b) -> (b -> [a]) -> [a] -> [b]
splitIntoEquivalenceClasses tryGetClass getClassMembers = 
  List.unfoldr tryGetNextState
  where
    tryGetNextState remainingItems =
      remainingItems
        & Maybe.mapMaybe tryGetClass
        & tryGetHead
        & Prelude.fmap (\c -> (c, remainingItems \\ getClassMembers c))

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
    Nothing -> getNeighbors board coord
  where
    getNeighbors board (x, y) =
      [(x - 1, y) | x > 1]
        ++ [(x + 1, y) | x < maxX]
        ++ [(x, y - 1) | y > 1]
        ++ [(x, y + 1) | y < maxY]
        & Set.fromList
      where
        (maxX, maxY) = getMaxCoord board

extractEmptyTerritoryAndColors :: [String] -> Set Coord -> (Set Coord, Set Color)
extractEmptyTerritoryAndColors board territory =
  let (emptyCoords, occupiedCoords) =
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

tryGetColor :: [String] -> Coord -> Maybe Color
tryGetColor board (x, y)
  | x <= 0 = error $ printf "x = %d but must be >= 1" x
  | y <= 0 = error $ printf "y = %d but must be >= 1" y
  | x - 1 > maxX = error $ printf "x = %d but must be <= %d" x maxX
  | y - 1 > maxY = error $ printf "y = %d but must be <= %d" y maxY
  | otherwise = board !! (y - 1) !! (x - 1) & parseColorOrEmpty
  where
    (maxX, maxY) = getMaxCoord board

getAllCoords :: [String] -> [Coord]
getAllCoords board =
  [(x, y) | x <- [1 .. maxX], y <- [1 .. maxY]]
  where
    (maxX, maxY) = getMaxCoord board

getMaxCoord :: [String] -> Coord
getMaxCoord board =
  (maxX, maxY)
  where
    maxY = List.length board
    maxX =
      if maxY > 0
        then List.length $ List.head board
        else 0

parseColorOrEmpty :: Char -> Maybe Color
parseColorOrEmpty 'B' = Just Black
parseColorOrEmpty 'W' = Just White
parseColorOrEmpty ' ' = Nothing
parseColorOrEmpty char = error $ printf "Invalid color: %s" char

tryGetHead :: [a] -> Maybe a
tryGetHead list =
  list
    & List.uncons
    & Prelude.fmap fst
