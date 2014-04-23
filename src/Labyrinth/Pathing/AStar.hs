{-# LANGUAGE ScopedTypeVariables, ViewPatterns #-}
{-|
Module      : Labyrinth.AStar
Description : pathfinding
Copyright   : (c) deweyvm 2014
License     : MIT
Maintainer  : deweyvm
Stability   : experimental
Portability : unknown

Implementation of the A* (A star) pathfinding algorithm.
-}
module Labyrinth.Pathing.AStar(pfind) where

import Prelude hiding(elem, all)
import qualified Data.Map as Map
import qualified Data.PSQueue as Q
import qualified Data.Set as Set
import Labyrinth.PathGraph
import Labyrinth.Pathing.Util

data Path b = Path (Set.Set b)       -- closed set
                   (Map.Map b Float) -- g score
                   (Q.PSQ b Float)   -- f score, open set
                   (Map.Map b b)     -- path so far
                   b                 -- goal node

mkPath :: (Metric a, Ord a) => a -> a -> Path a
mkPath start goal = Path Set.empty
                         (Map.singleton start 0)
                         (Q.singleton start $ guessLength start goal)
                         Map.empty
                         goal

pathHelper :: forall a b.(Ord b, Metric b, PathGraph a b) => a -> Path b -> Either String [b]
pathHelper graph (Path closedSet gs fsop path goal) =
    case Q.minView fsop of
        Just (current, newOpen) -> processCurrent (Q.key current) newOpen
        Nothing -> Left "Found no path"
    where processCurrent :: b -> Q.PSQ b Float -> Either String [b]
          processCurrent currentNode open =
              let newClosed = Set.insert currentNode closedSet in
              if currentNode == goal
              then Right $ rewindPath path goal []
              else let ns = getNeighbors graph currentNode
                       (gs', fsop', path') = foldl (updatePath goal currentNode newClosed) (gs, open, path) ns in
                       pathHelper graph (Path newClosed gs' fsop' path' goal)


updatePath :: (Ord b, Metric b)
           => b
           -> b
           -> Set.Set b
           -> (Map.Map b Float, Q.PSQ b Float, Map.Map b b)
           -> (b, Float)
           -> (Map.Map b Float, Q.PSQ b Float, Map.Map b b)
updatePath goal current closed s@(gs, fs, p) (nnode, ncost) =
    if Set.member nnode closed
    then s
    else case Map.lookup current gs of
        Just g ->
            let g' = g + ncost in
            if g' < g || not (qMember nnode fs)
            then let f = (g' + guessLength nnode goal) in
                 let newPath = Map.insert nnode current p in
                 let newGs = Map.insert nnode g' gs in
                 let newFsop = Q.insert nnode f fs in
                 (newGs, newFsop, newPath)
            else s
        Nothing -> error "data structure inconsistent"

-- | Find a shortest path from the start node to the goal node.
pfind :: (Ord b, Metric b, PathGraph a b)
      => a                 -- ^ The graph to be traversed
      -> b                 -- ^ The start node
      -> b                 -- ^ The goal node
      -> Either String [b] {- ^ Either a string explaining why a path could
                                not be found, or the found shortest path in
                                order from start to goal.-}
pfind graph start goal = pathHelper graph $ mkPath start goal
