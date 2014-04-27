{-# LANGUAGE ScopedTypeVariables, ViewPatterns, FlexibleContexts #-}
{-|
Module      : Labyrinth.Pathing.Util
Description : pathfinding utilities
Copyright   : (c) deweyvm 2014
License     : MIT
Maintainer  : deweyvm
Stability   : experimental
Portability : unknown

Functions shared across different pathfinding algorithms.
-}
module Labyrinth.Pathing.Util where
import Control.Applicative
import Control.Arrow
import Data.Maybe
import qualified Data.PSQueue as Q
import qualified Data.Map as Map
import Labyrinth.Util


expandPath :: (a -> a -> [a]) -> [a] -> [a]
expandPath _ [] = []
expandPath f xs = concat $ uncurry f <$> zip xs (tail xs)

expand :: Point -> Point -> [Point]
expand (px, py) (qx, qy) =
    let dx = qx - px
        dy = qy - py
        sx = signum dx
        sy = signum dy
        n = max (abs dx) (abs dy)
        iter s = (take (n+1) $ iterate (+s) 0) in
    ((+px) *** (+py)) <$> zip (iter sx) (iter sy)

rewindPath :: Ord a => Map.Map a a -> a -> [a] -> [a]
rewindPath path end sofar =
    case Map.lookup end path of
        Just next -> rewindPath path next (end:sofar)
        Nothing -> sofar

euclid :: Point -> Point -> Float
euclid (i, j) (x, y) =  (sqrt (xx + yy))
        where xx = sq (x - i)
              yy = sq (y - j)
              sq = (** 2) . fromIntegral

qMember :: (Ord a, Ord b) => a -> Q.PSQ a b -> Bool
qMember = isJust .: Q.lookup

