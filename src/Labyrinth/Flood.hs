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
module Labyrinth.Flood(floodFill, floodAll, simpleFloodAll, getDepth, getNode) where

import Control.Monad
import Control.Applicative
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Labyrinth.PathGraph
data FloodNode a = FloodNode Int a

-- | Returns the distance a flooded node is from the origin node
getDepth :: FloodNode a -> Int
getDepth (FloodNode i _) = i

-- | Returns the coordinate of a flooded node
getNode :: FloodNode a -> a
getNode (FloodNode _ x) = x

data Flood a = Flood (Set.Set (FloodNode a)) (Seq.Seq a)

instance Eq a => Eq (FloodNode a) where
    (FloodNode _ x) == (FloodNode _ y) = x == y

instance Ord a => Ord (FloodNode a) where
    compare (FloodNode _ x) (FloodNode _ y) = compare x y

mkFlood :: a -> Flood a
mkFlood = liftM2 Flood (Set.singleton . (FloodNode 0)) Seq.singleton

-- | Floods a graph starting from the given node
floodFill :: (PathGraph a b, Ord b)
          => a                     -- ^ the graph to be flooded
          -> b                     -- ^ the seed point
          -> Set.Set (FloodNode b) -- ^ the set of flooded nodes
floodFill graph pt = floodHelper graph 0 $ mkFlood pt


floodHelper :: (PathGraph a b, Ord b)
            => a
            -> Int
            -> Flood b
            -> Set.Set (FloodNode b)
floodHelper     _     _ (Flood pts (Seq.viewl -> Seq.EmptyL)) = pts
floodHelper graph depth (Flood pts (Seq.viewl -> pt Seq.:< work)) =
    floodHelper graph (depth + 1) (Flood full q)
    where q = (Seq.fromList ns) Seq.>< work
          full = Set.union pts (Set.fromList lst)
          lst = zipWith ($) (FloodNode <$> (repeat depth)) ns
          ns = filter notMember $ fst <$> getNeighbors graph pt
          notMember x = Set.notMember (FloodNode 0 x) pts

-- | Floods all given passable nodes on a given graph
floodAll :: (PathGraph a b, Ord b)
         => a                       -- ^ the graph to be flooded
         -> Set.Set b               -- ^ the set of all open nodes
         -> [Set.Set (FloodNode b)] -- ^ the resulting flooded regions
floodAll graph open = floodAllHelper graph open []


floodAllHelper :: (PathGraph a b, Ord b)
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
    where nodeDiff r s = Set.difference r (Set.map getNode s)

{- | Floods all regions of r graph reachable from the given open nodes
     Discards depth, leaving only the filled coordinates-}
simpleFloodAll :: (PathGraph a b, Ord b)
               => a           -- ^ the graph to be flooded
               -> Set.Set b   -- ^ the set of all open nodes
               -> [Set.Set b] -- ^ the resulting flooded regions
simpleFloodAll graph open =
    Set.map getNode <$> floodAll graph open
