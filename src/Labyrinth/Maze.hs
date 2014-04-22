{-# LANGUAGE ScopedTypeVariables, ViewPatterns, MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}
{-|
Module      : Labyrinth.PathGraph
Description : maze typeclass
Copyright   : (c) deweyvm 2014
License     : MIT
Maintainer  : deweyvm
Stability   : experimental
Portability : unknown

A maze and associated typeclass for doing fancier things with specific
types of graphs
-}
module Labyrinth.Maze(
    Maze(..),
    Node(..),
    isNode
) where

data Node a = Node a
            | Solid
            | OutOfBounds

isNode :: Node a -> Bool
isNode (Node _) = True
isNode _ = False


{- | A graph where some nodes are passable but others are not.
   A maze also has a notion a border, made up of nodes which
   are considered out of bounds. -}
class Maze a b c | a -> b where
    -- | Get adjacent nodes to a given vertex
    getAdjacent :: a
                -> c
                -> [(Node b, Float)]

    -- | Returns the passability of the given node.
    isPassable :: a
               -> c
               -> Bool
