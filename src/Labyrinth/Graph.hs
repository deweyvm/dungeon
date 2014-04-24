{-# LANGUAGE ScopedTypeVariables, ViewPatterns, MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}
{-|
Module      : Labyrinth.Graph
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
    Open(..)
) where

{- | A graph typeclass.
    a - the underlying collection type
    b - the element representing nodes in the graph
    c - the coordinate indexing nodes
   -}
class Graph a b c where
    getNeighbors :: a b          -- ^ the underlying collection
                 -> c            -- ^ the coordinate of a node
                 -> [(c, Float)] -- ^ a list of (node coordinate,cost) tuples


-- | Glass signifying if an object is "passable" for mazes.
class Open a where
    isOpen :: a -> Bool

