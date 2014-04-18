{-# LANGUAGE ScopedTypeVariables, ViewPatterns, MultiParamTypeClasses #-}
{-|
Module      : Labyrinth.PathGraph
Description : graph typeclass
Copyright   : (c) deweyvm 2014
License     : MIT
Maintainer  : deweyvm
Stability   : experimental
Portability : unknown

A graph and associated typeclasses for doing flood fills, pathfinding, etc
independent of data structure.
-}
module Labyrinth.PathGraph(PathGraph(..), Metric(..), Open(..)) where

-- | A graph with underlying type a and coordinate type b
class PathGraph a b where
    getNeighbors :: a            -- ^ the underlying collection
                 -> b            {- ^ the coordinate from which to
                                      get neighboring nodes -}
                 -> [(b, Float)] -- ^ a list of node,cost tuples

-- | A metric for measuring the distance between two objects
class Metric a where
    guessLength :: a -> a -> Float

-- | Whether or not an object is considered passable
class Open a where
    isOpen :: a -> Bool

