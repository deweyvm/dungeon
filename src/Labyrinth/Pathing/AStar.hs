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
import Labyrinth.Graph
import Labyrinth.Util
import Labyrinth.Pathing.Util

data Path b = Path (Set.Set b)       -- closed set
                   (Map.Map b Float) -- g score
                   (Q.PSQ b Float)   -- f score, open set
                   (Map.Map b b)     -- path so far
                   b                 -- goal node

mkPath :: Ord a => Heuristic a -> a -> a -> Path a
mkPath l start goal = Path Set.empty
                         (Map.singleton start 0)
                         (Q.singleton start $ l start goal)
                         Map.empty
                         goal

pathHelper :: forall a b c.(Ord c, Graph a b c)
           => Heuristic c
           -> a b
           -> Path c
           -> Either String [c]
pathHelper h graph (Path closedSet gs fsop path goal) =
    case Q.minView fsop of
        Just (current, newOpen) -> processCurrent (Q.key current) newOpen
        Nothing -> Left "Found no path"
    where processCurrent :: c -> Q.PSQ c Float -> Either String [c]
          processCurrent currentNode open =
              let newClosed = Set.insert currentNode closedSet in
              if currentNode == goal
              then Right $ rewindPath path goal []
              else let ns = getNeighbors graph currentNode
                       (gs', fsop', path') = foldl (updatePath h goal currentNode newClosed) (gs, open, path) ns in
                       pathHelper h graph (Path newClosed gs' fsop' path' goal)


updatePath :: Ord a
           => Heuristic a
           -> a
           -> a
           -> Set.Set a
           -> (Map.Map a Float, Q.PSQ a Float, Map.Map a a)
           -> (a, Float)
           -> (Map.Map a Float, Q.PSQ a Float, Map.Map a a)
updatePath h goal current closed s@(gs, fs, p) (nnode, ncost) =
    if Set.member nnode closed
    then s
    else case Map.lookup current gs of
        Just g ->
            let g' = g + ncost in
            if g' < g || not (qMember nnode fs)
            then let f = (g' + h nnode goal) in
                 let newPath = Map.insert nnode current p in
                 let newGs = Map.insert nnode g' gs in
                 let newFsop = Q.insert nnode f fs in
                 (newGs, newFsop, newPath)
            else s
        Nothing -> error "data structure inconsistent"

{- | Find a path from the start node to the goal node.
     Given an admissible heuristic, this will also be a shortest path.-}
pfind :: (Ord c, Graph a b c)
      => Heuristic c       -- ^ The pathfinding heuristic
      -> a b               -- ^ The graph to be traversed
      -> c                 -- ^ The start node
      -> c                 -- ^ The goal node
      -> Either String [c] {- ^ Either a string explaining why a path could
                                not be found, or the found shortest path in
                                order from start to goal.-}
pfind h graph start goal = pathHelper h graph $ mkPath h start goal
