{-# LANGUAGE ScopedTypeVariables, ViewPatterns, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module      : Labyrinth.Instances
Description : graph instances
Copyright   : (c) deweyvm 2014
License     : MIT
Maintainer  : deweyvm
Stability   : experimental
Portability : unknown

Graph and maze (and related) instances.
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

instance Invertible Bool where
    invert = not

neighbors8 :: [Point]
neighbors8 = ns
    where ns = [ (x, y) | x <- [-1..1], y <- [-1..1], not (x == 0 && y == 0) ]

-- | Retrieves the 8 neighbors of a 2d point.
getNeighbors8 :: Point -> [Point]
getNeighbors8 (i, j) = ((i +) *** (j +)) <$> neighbors8

instance Open a => Graph Array2d a Point where
   getNeighbors g pt = ap (,) (euclid pt) <$> ns
       where ns = filter open $ getNeighbors8 pt
             open = (any isOpen) . (geti g)


infinity :: Float
infinity = 1 / 0

instance Open a => Maze Array2d a Point where
    getAdjacent g pt = mapNode <$> ns
       where ns :: [Node a Point]
             ns = getNode g <$> getNeighbors8 pt
             mapNode :: Node a Point -> (Node a Point, Float)
             mapNode n@(Node _ q) = (n, euclid pt q)
             mapNode n = (n, infinity)
    getNode g pt = case geti g pt of
                        Just x | isOpen x -> Node x pt
                        Just _ -> Solid pt
                        _ -> OutOfBounds pt
    isHardBound (Array2d cols rows _) (x, y) =
        x == 0 || y == 0 || x == cols - 1 || y == rows - 1

instance Border Array2d a Point where
    addBorder arr@(Array2d cols rows _) bk =
        let unshift = (subtract 1) *** (subtract 1) in
        let get pt = case geti arr (unshift pt) of
                         Just x -> x
                         Nothing -> bk in
        let border = tabulate (cols + 2) (rows + 2) bk get in
        (border, unshift)





