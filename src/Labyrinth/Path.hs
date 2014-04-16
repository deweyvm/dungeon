{-# LANGUAGE ScopedTypeVariables, ViewPatterns, MultiParamTypeClasses #-}
{-|
Module      : Labyrinth.Path
Description : pathfinding
Copyright   : (c) deweyvm 2014
License     : MIT
Maintainer  : deweyvm
Stability   : experimental
Portability : unknown

Implementation of the A* pathfinding algorithm.
-}
module Labyrinth.Path(pfind, PathGraph(..), Metric(..), Solid(..)) where

import Prelude hiding(elem, all)
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.PSQueue as Q
import qualified Data.Set as Set
import Labyrinth.Util

class PathGraph a b where
    getNeighbors :: a -> b -> [(b, Float)]

class Metric a where
    guessLength :: a -> a -> Float

class Solid a where
    isSolid :: a -> Bool


mkPath :: (Metric a, Ord a) => a -> a -> Path a
mkPath initial goal = Path Set.empty
                           (Map.singleton initial 0)
                           (Q.singleton initial $ guessLength initial goal)
                           Map.empty
                           goal


data Path b = Path (Set.Set b)       -- closed set
                   (Map.Map b Float) -- g score
                   (Q.PSQ b Float)   -- f score, open set
                   (Map.Map b b)     -- path so far
                   b                 -- goal node

rewindPath :: Ord b => Map.Map b b -> b -> [b] -> [b]
rewindPath path end sofar =
    case Map.lookup end path of
        Just next -> rewindPath path next (end:sofar)
        Nothing -> sofar

pathHelper :: forall a b.(Ord b, Metric b, PathGraph a b) => a -> Path b -> Either String [b]
pathHelper coll (Path closedSet gs fsop path goal) =
    case Q.minView fsop of
        Just (current, newOpen) -> processCurrent (Q.key current) newOpen
        Nothing -> Left "Found no path"
    where processCurrent :: b -> Q.PSQ b Float -> Either String [b]
          processCurrent currentNode open =
              let newClosed = Set.insert currentNode closedSet in
              if currentNode == goal
              then Right $ rewindPath path goal []
              else let ns = getNeighbors coll currentNode
                       (gs', fsop', path') = foldl (updatePath goal currentNode newClosed) (gs, open, path) ns in
                       pathHelper coll (Path newClosed gs' fsop' path' goal)

qMember :: (Ord a, Ord b) => a -> Q.PSQ a b -> Bool
qMember = isJust .: Q.lookup

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

-- | Find /a/ shortest path from the initial node to the goal node
pfind :: (Ord b, Metric b, PathGraph a b)
      => a                 -- ^ The graph to be traversed
      -> b                 -- ^ The initial node
      -> b                 -- ^ The goal node
      -> Either String [b] {- ^ Either a string explaining why a path could
                                not be found, or the found shortest path in
                                order from initial to goal.-}
pfind graph initial goal = pathHelper graph $ mkPath initial goal
