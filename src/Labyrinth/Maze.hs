{-# LANGUAGE ScopedTypeVariables, ViewPatterns, MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}
{-|
Module      : Labyrinth.Maze
Description : maze typeclass
Copyright   : (c) deweyvm 2014
License     : MIT
Maintainer  : deweyvm
Stability   : experimental
Portability : unknown

A maze and associated typeclass for doing fancier things with specific
types of graphs.
-}
module Labyrinth.Maze(
    Maze(..),
    Node(..),
    Invertible(..),
    Border(..),
    isNode,
    getCoord
) where
import Labyrinth.Graph
data Node a b = Node a b
              | Solid b
              | OutOfBounds b
    deriving (Ord, Eq)


isNode :: Node a b -> Bool
isNode (Node _ _) = True
isNode _ = False

getCoord :: Node a b -> b
getCoord (Node _ x) = x
getCoord (Solid x) = x
getCoord (OutOfBounds x) = x
{- | A graph where some nodes are passable but others are not.
   A maze also has a notion a border, made up of nodes which
   are considered out of bounds.
    a - the underlying collection type
    b - the element representing nodes in the graph
    c - the coordinate indexing nodes -}
class (Functor a, Graph a b c) => Maze a b c | a -> c where
    -- | Get adjacent nodes to a given vertex
    getAdjacent :: a b -> c -> [(Node b c, Float)]
    getNode :: a b -> c -> Node b c
    isHardBound :: a b -> c -> Bool
    -- | Returns the passability of the given node.
    isPassable :: a b -> c -> Bool
    isPassable g coord = (isNode . snd) $ (,) coord $ getNode g coord

{- | A graph with a notion of a border which can be expanded.
     a - the underlying collection type
     b - the element representing nodes in the graph
     c - the coordinate indexing nodes -}
class Border a b c | a -> c where
    addBorder :: a b
              -> b
              -> (a b, c -> c)

-- | Represents (bijectively) invertible elements.
class Invertible a where
    invert :: a -> a
