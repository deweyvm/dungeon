{-# LANGUAGE ScopedTypeVariables, ViewPatterns, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module      : Labyrinth.PathGraph
Description : graph instances
Copyright   : (c) deweyvm 2014
License     : MIT
Maintainer  : deweyvm
Stability   : experimental
Portability : unknown

A graph and associated typeclasses for doing flood fills, pathfinding, etc
independent of data structure.
-}
module Labyrinth.Instances() where

import Prelude hiding(any)
import Control.Arrow((***))
import Control.Applicative
import Control.Monad
import Data.Foldable(any)
import Labyrinth.Data.Array2d
import Labyrinth.Pathing.Util
import Labyrinth.Graph
import Labyrinth.Maze
import Labyrinth.Util

instance Open Bool where
    isOpen = id

neighbors8 :: [Point]
neighbors8 = ns
    where ns = [ (x, y) | x <- [-1..1], y <- [-1..1], not (x == 0 && y == 0) ]

-- | Retrieves the 8 neighbors of a 2d point
getNeighbors8 :: Point -> [Point]
getNeighbors8 (i, j) = ((i +) *** (j +)) <$> neighbors8
-- (\(x, y)-> (i + x, j + y)) === (i +) *** (j +)


instance Open a => Graph (Array2d a) Point where
   getNeighbors g pt = ap (,) (euclid pt) <$> ns
       where ns = filter open $ getNeighbors8 pt
             open = (any isOpen) . (geti g)


infinity :: Float
infinity = 1 / 0
instance Open a => Maze (Array2d a) a Point where
    getAdjacent g pt = undefined mapNode <$> ns
       where ns = getNode g <$> getNeighbors8 pt
             mapNode (qt, n) = (n, euclid pt qt)
             mapNode (_, x) = (x, infinity)
    getNode g pt = (case geti g pt of
                               Just x | isOpen x -> Node x
                               Just _ -> Solid
                               _      -> OutOfBounds)
    isPassable g pt = (isNode . snd) $ (,) pt $ getNode g pt

instance Heuristic Point where
    guessLength = (/ 1.5) .: euclid
