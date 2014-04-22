{-# LANGUAGE ScopedTypeVariables, ViewPatterns, BangPatterns #-}
{-|
Module      : Labyrinth.Flood
Description : flood filling
Copyright   : (c) deweyvm 2014
License     : MIT
Maintainer  : deweyvm
Stability   : experimental
Portability : unknown

Implementation of flood fill for arbitrary graphs.
-}
module Labyrinth.Flood(
    floodFill,
    floodAll,
    getNode
) where

import Control.Applicative
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Data.List as List
import Labyrinth.Graph
import Labyrinth.Maze

data Flood a b = Flood (Set.Set a) (Seq.Seq b)

mkFlood :: a -> b -> Flood a b
mkFlood x y = Flood (Set.singleton x) (Seq.singleton y)

floodMaze :: (Maze a b c, Ord b, Ord c)
          => a b
          -> c
          -> Set.Set (Node b c)
floodMaze g pt = floodMazeHelper g $ mkFlood (getNode g pt) pt

floodMazeHelper :: (Maze a b c, Ord b, Ord c)
                => a b
                -> Flood (Node b c) c -- b == Bool, c == Point
                -> Set.Set (Node b c)
floodMazeHelper     _ (Flood pts (Seq.viewl -> Seq.EmptyL)) = pts
floodMazeHelper graph (Flood pts (Seq.viewl -> pt Seq.:< work)) =
    floodMazeHelper graph (Flood full q)
    where q = (Seq.fromList (getCoord <$> newWork)) Seq.>< work
          full = Set.union pts (Set.fromList (fst <$> adj))
          newWork = filter notMember $ fst <$> open
          open= List.filter (isNode.fst) adj
          adj = getAdjacent graph pt
          notMember x = Set.notMember x pts




-- | Floods a graph starting from the given node
floodFill :: (Graph a b, Ord b)
          => a         -- ^ the graph to be flooded
          -> b         -- ^ the seed point
          -> Set.Set b -- ^ the set of flooded nodes
floodFill graph pt = floodHelper graph $ mkFlood pt pt


floodHelper :: (Graph a b, Ord b)
            => a
            -> Flood b b
            -> Set.Set b
floodHelper     _ (Flood pts (Seq.viewl -> Seq.EmptyL)) = pts
floodHelper graph (Flood pts (Seq.viewl -> pt Seq.:< work)) =
    floodHelper graph (Flood full q)
    where q = (Seq.fromList ns) Seq.>< work
          full = Set.union pts (Set.fromList ns)
          ns = filter notMember $ fst <$> getNeighbors graph pt
          notMember x = Set.notMember x pts

-- | Floods all given passable regions on a given graph.
floodAll :: (Graph a b, Ord b)
         => a           -- ^ the graph to be flooded
         -> Set.Set b   -- ^ the set of all open nodes
         -> [Set.Set b] -- ^ the resulting flooded regions
floodAll graph open = floodAllHelper graph open []


floodAllHelper :: (Graph a b, Ord b)
               => a
               -> Set.Set b
               -> [Set.Set b]
               -> [Set.Set b]
floodAllHelper graph open sofar =
    case Set.minView open of
        Just (x, _) -> let filled = floodFill graph x in
                       let newOpen = Set.difference open filled in
                       floodAllHelper graph newOpen (filled:sofar)
        Nothing -> sofar



-- invert grid
-- flood fill exterior (can always start at (0,0))
-- any wall that is touched that is not out of bounds is a boundary
-- any wall touching the edge of the map is a boundary
computeBorder :: (Invertible b, Ord c, Maze a b c) => a b => Set.Set c
computeBorder m =
    let inverted = invert <$> m in
    undefined
