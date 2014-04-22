{-# LANGUAGE ScopedTypeVariables, ViewPatterns, MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}
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
module Labyrinth.Graph(
    Graph(..),
    Heuristic(..),
    Open(..)
) where

-- | A graph with underlying type a and coordinate type b
class Graph a b where
    getNeighbors :: a            -- ^ the underlying collection
                 -> b            -- ^ the coordinate of a node
                 -> [(b, Float)] -- ^ a list of (node coordinate,cost) tuples


-- | A metric for measuring the distance between two objects
class Heuristic a where
    guessLength :: a -> a -> Float

-- | Glass signifying if an object is "passable" for mazes
class Open a where
    isOpen :: a -> Bool
