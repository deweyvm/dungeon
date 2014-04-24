{-# LANGUAGE ScopedTypeVariables, ViewPatterns, FlexibleContexts #-}
{-|
Module      : Labyrinth.Pathing.Dijkstra
Description : dijkstra's algorithm
Copyright   : (c) deweyvm 2014
License     : MIT
Maintainer  : deweyvm
Stability   : experimental
Portability : unknown

Dijkstra's search algorithm.
-}
module Labyrinth.Pathing.Dijkstra(pfind) where

import qualified Data.PSQueue as Q
import qualified Data.Map as Map
import Labyrinth.Pathing.Util
import Labyrinth.Graph

data Path a = Path (Map.Map a Float) -- dist
                   (Map.Map a a)     -- prev
                   (Q.PSQ a Float)   -- prio

infinity :: Float
infinity = 1 / 0

getDist :: Ord a => Map.Map a Float -> a -> Float
getDist m x =
    case Map.lookup x m of
        Just y -> y
        Nothing -> infinity

mkPath :: Ord a => a -> Path a
mkPath x = Path (Map.singleton x 0)
                (Map.empty)
                (Q.singleton x 0)

dijkstraHelper :: (Ord c, Graph a b c)
               => a b
               -> c
               -> c
               -> Path c
               -> Either String [c]
dijkstraHelper g start goal (Path dist prev prio) =
    case Q.minView prio of
        Just (current, newPrio) ->
            let node = Q.key current in
            let ns = getNeighbors g node in
            let newP = foldl (processNeighbor node) (Path dist prev newPrio) ns in
            if node == goal
            then Right $ rewindPath prev goal []
            else dijkstraHelper g start goal newP
        Nothing -> Left "Found no path"
    where processNeighbor u p@(Path dist' prev' prio') (v, c)=
            let alt = (getDist dist' u) + c in
            let d = getDist dist' v in
            if alt < d
            then let newDist = Map.insert v alt dist' in
                 let newPrev = Map.insert v u prev' in
                 let newPrio = Q.alter (\_ -> Just alt) v prio' in
                 Path newDist newPrev newPrio
            else p

-- | Find a shortest path from the given start node to the goal node.
pfind :: (Ord c, Graph a b c)
      => a b               -- ^ The graph to be traversed
      -> c                 -- ^ The start node
      -> c                 -- ^ The goal node
      -> Either String [c] {- ^ Either a string explaining w
                                not be found, or the found s
                                order from start to goal.-}
pfind g start goal = dijkstraHelper g start goal $ mkPath start
