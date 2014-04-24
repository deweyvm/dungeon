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
    getNode,
    computeBorder,
    getDiameter
) where

import Control.Applicative
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Data.List as List
import qualified Labyrinth.Pathing.Dijkstra as D
import Labyrinth.Util
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




-- | Floods a graph starting from the given node.
floodFill :: (Graph a b c, Ord c)
          => a b       -- ^ the graph to be flooded
          -> c         -- ^ the seed point
          -> Set.Set c -- ^ the set of flooded nodes
floodFill graph pt = floodHelper graph $ mkFlood pt pt


floodHelper :: (Graph a b c, Ord c)
            => a b
            -> Flood c c
            -> Set.Set c
floodHelper     _ (Flood pts (Seq.viewl -> Seq.EmptyL)) = pts
floodHelper graph (Flood pts (Seq.viewl -> pt Seq.:< work)) =
    floodHelper graph (Flood full q)
    where q = (Seq.fromList ns) Seq.>< work
          full = Set.union pts (Set.fromList ns)
          ns = filter notMember $ fst <$> getNeighbors graph pt
          notMember x = Set.notMember x pts

-- | Floods all given passable regions on a given graph.
floodAll :: (Graph a b c, Ord c)
         => a b         -- ^ the graph to be flooded
         -> Set.Set c   -- ^ the set of all open nodes
         -> [Set.Set c] -- ^ the resulting flooded regions
floodAll graph open = floodAllHelper graph open []


floodAllHelper :: (Graph a b c, Ord c)
               => a b
               -> Set.Set c
               -> [Set.Set c]
               -> [Set.Set c]
floodAllHelper graph open sofar =
    case Set.minView open of
        Just (x, _) -> let filled = floodFill graph x in
                       let newOpen = Set.difference open filled in
                       floodAllHelper graph newOpen (filled:sofar)
        Nothing -> sofar



{- | Compute the border of a maze. The border is defined to be any open
     space touching an impassable node, where that impassable node has a
     chain of impassable nodes leading out of bounds. -}
computeBorder :: (Border a b c, Maze a b c, Invertible b, Ord b, Ord c)
              => a b
              -> b
              -> c
              -> Set.Set c
computeBorder m blank seed =
    let (c, revert) = addBorder m blank in
    let nodes = floodMaze (invert <$> c) seed in
    let mapped = catMaybes $ Set.foldr (\node acc -> f node : acc) [] nodes in
    Set.fromList $ map revert mapped
    where f (Node _ _) = Nothing
          f (OutOfBounds p) = Just p
          f (Solid p) = Just p



getDiameter :: (Invertible b, Ord b, Ord c, Border a b c, Maze a b c)
            => a b
            -> b
            -> c
            -> (Int, c, c, [c])
getDiameter g blank pt =
    let borders = Set.toList $ computeBorder g blank pt in
    let pairs = [ (x, y) | x <- borders, y <- borders, x /= y] in
    --let !_ = myTrace $ pairs in
    --fixme, do not ignore failures as they should be impossible!
    let e = catEithers $ (\(start, goal) -> D.pfind g start goal) <$> pairs in
    let paths = snd e in

    let lengths = (\(l, path) -> (l, head path, last path, path)) <$> zip (length <$> paths) paths in
    List.maximumBy (\(l, _, _, _) (r, _, _, _) -> l `compare` r) lengths
