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
import Data.Maybe
import qualified Data.PSQueue as Q
import qualified Data.Map as Map
import Labyrinth.Util

rewindPath :: Ord b => Map.Map b b -> b -> [b] -> [b]
rewindPath path end sofar =
    case Map.lookup end path of
        Just next -> rewindPath path next (end:sofar)
        Nothing -> sofar


euclid :: Point -> Point -> Float
euclid (i, j) (x, y) =  (sqrt (xx + yy))
        where xx = sq (x - i)
              yy = sq (y - j)
              sq= (** 2) . fromIntegral

qMember :: (Ord a, Ord b) => a -> Q.PSQ a b -> Bool
qMember = isJust .: Q.lookup
