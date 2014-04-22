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
    simpleFloodAll,
    getDepth,
    getNode
) where

import Control.Applicative
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Labyrinth.Graph
import Labyrinth.Maze

data FloodNode a = FloodNode Int a

-- | Returns the distance a flooded node is from the origin node
getDepth :: FloodNode a -> Int
getDepth (FloodNode i _) = i

-- | Returns the coordinate of a flooded node
getCoord :: FloodNode a -> a
getCoord (FloodNode _ x) = x

instance Eq a => Eq (FloodNode a) where
    (FloodNode _ x) == (FloodNode _ y) = x == y

instance Ord a => Ord (FloodNode a) where
    compare (FloodNode _ x) (FloodNode _ y) = compare x y

data Flood a = Flood (Set.Set (FloodNode a)) (Seq.Seq (FloodNode a))

mkFlood :: a -> Flood a
mkFlood x = Flood (Set.singleton . mkNode $ x) (Seq.singleton . mkNode $ x)
    where mkNode = FloodNode 0

-- | Floods a graph starting from the given node
floodFill :: (Graph a b, Ord b)
          => a                     -- ^ the graph to be flooded
          -> b                     -- ^ the seed point
          -> Set.Set (FloodNode b) -- ^ the set of flooded nodes
floodFill graph pt = floodHelper graph $ mkFlood pt


floodHelper :: (Graph a b, Ord b)
            => a
            -> Flood b
            -> Set.Set (FloodNode b)
floodHelper     _ (Flood pts (Seq.viewl -> Seq.EmptyL)) = pts
floodHelper graph (Flood pts (Seq.viewl -> (FloodNode depth pt) Seq.:< work)) =
    floodHelper graph (Flood full q)
    where q = (Seq.fromList lst) Seq.>< work
          full = Set.union pts (Set.fromList lst)
          lst = zipWith ($) (FloodNode <$> (repeat (depth + 1))) ns
          ns = filter notMember $ fst <$> getNeighbors graph pt
          notMember x = Set.notMember (FloodNode 0 x) pts

-- | Floods all given passable regions on a given graph.
floodAll :: (Graph a b, Ord b)
         => a                       -- ^ the graph to be flooded
         -> Set.Set b               -- ^ the set of all open nodes
         -> [Set.Set (FloodNode b)] -- ^ the resulting flooded regions
floodAll graph open = floodAllHelper graph open []


floodAllHelper :: (Graph a b, Ord b)
               => a
               -> Set.Set b
               -> [Set.Set (FloodNode b)]
               -> [Set.Set (FloodNode b)]
floodAllHelper graph open sofar =
    case Set.minView open of
        Just (x, _) -> let filled = floodFill graph x in
                       let newOpen = nodeDiff open filled in
                       floodAllHelper graph newOpen (filled:sofar)
        Nothing -> sofar
    where nodeDiff r s = Set.difference r (Set.map getCoord s)

{- | Floods all regions of r graph reachable from the given open nodes
     Discards depth, leaving only the filled coordinates-}
simpleFloodAll :: (Graph a b, Ord b)
               => a           -- ^ the graph to be flooded
               -> Set.Set b   -- ^ the set of all open nodes
               -> [Set.Set b] -- ^ the resulting flooded regions
simpleFloodAll graph open =
    Set.map getCoord <$> floodAll graph open



-- pad grid with open space
-- invert grid
-- flood fill exterior (can always start at (0,0))
-- any wall that is touched that is not out of bounds is a
computeBorder :: (Invertible b, Ord c, Maze a b c) => a
computeBorder = undefined
