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

class PathGraph a b where
    getNeighbors :: a -> b -> [(b, Float)]

class Metric a where
    guessLength :: a -> a -> Float

class Open a where
    isOpen :: a -> Bool

