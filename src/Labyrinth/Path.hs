{-# LANGUAGE ScopedTypeVariables, ViewPatterns, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-|
Module      : Labyrinth.Path
Description : pathfinding
Copyright   : (c) deweyvm 2014
License     : GPL-3
Maintainer  : deweyvm
Stability   : experimental
Portability :

Implementation of the A* pathfinding algorithm.
-}
module Labyrinth.Path(pfind) where

import Prelude hiding(elem, all)
import Control.Applicative
import Data.Maybe
import qualified Data.Map as Map
import Data.Foldable(minimumBy)
--import qualified Data.PSQueue as Q
import qualified Data.Set as Set
import Labyrinth.Data.Array2d(Array2d,Point,geti,zipWithIndex)
import qualified Labyrinth.Flood as F
--import Debug.Trace


class PathGraph a b where
    getNeighbors :: a -> b -> [(b, Float)]

class Metric a where
    guessLength :: a -> a -> Float

class Solid a where
    isSolid :: a -> Bool

instance Solid Bool where
    isSolid = id

instance Solid a => PathGraph (Array2d a) Point where
    getNeighbors arr pt = (\pp -> (pp, guessLength pp pt)) <$> (fst <$> filtered)
        where filtered = filter (isSolid . snd) ns
              ns = catMaybes $ (geti (zipWithIndex arr)) <$> F.getNeighbors pt

instance Metric Point where
    guessLength (i, j) (x, y) = sqrt (xx + yy)
        where xx = sq (x - i)
              yy = sq (y - j)
              sq = (** 2) . fromIntegral
--f x = g x + h x
--g x = cost so far
--h x = guess cost of the path
pfind :: forall a b.(Ord b, Metric b, Show b, PathGraph a b) => a -> b -> b -> Either String [b]
pfind graph start end = pathHelper graph $ path
    where path :: Path b
          path = Path Set.empty
                      (Set.singleton start)
                      (Map.singleton start 0)
                      (Map.singleton start $ guessLength start end)
                      Map.empty
                      end

data Path b = Path (Set.Set b)       -- ClosedSet
                   (Set.Set b)       -- OpenSet
                   (Map.Map b Float) -- g scoroe
                   (Map.Map b Float) -- f score
                   (Map.Map b b)     -- PathSoFar
                   b                 -- goal node

rewindPath :: Ord b => Map.Map b b -> b -> [b] -> [b]
rewindPath path end sofar =
    case Map.lookup end path of
        Just next -> rewindPath path next (end:sofar)
        Nothing -> sofar


getMin :: forall a b.(Ord a, Ord b) => Map.Map a b -> Set.Set a -> Maybe (a, Set.Set a)
getMin fs set =
    let r = Set.toList $ Set.map (\s -> ((,) s) <$> (Map.lookup s fs)) set
        lst :: [(a, b)]
        lst = catMaybes $ r in
    if length lst == 0
    then Nothing
    else let elt = fst (minimumBy (\(_, y1) (_, y2) -> compare y1 y2) lst) in
         Just (elt, Set.delete elt set)


pathHelper :: forall a b.(Ord b, Metric b, Show b, PathGraph a b) => a -> Path b -> Either String [b]
pathHelper coll (Path closedSet openSet gs fs path goal) =
    case getMin fs openSet of
        Just (current, newOpen) -> processCurrent current newOpen
        Nothing -> Left "Found no path"
    where processCurrent :: b -> Set.Set b -> Either String [b]
          processCurrent currentNode open =
              let newClosed = Set.insert currentNode closedSet in
              if currentNode == goal
              then Right $ rewindPath path goal []
              else let ns = getNeighbors coll currentNode
                       (gs', fs', path', open') = foldl (updatePath goal currentNode newClosed) (gs, fs, path, open) (fst <$> ns) in
                       pathHelper coll (Path newClosed open' gs' fs' path' goal)


updatePath :: (Ord b, Metric b)
           => b
           -> b
           -> Set.Set b
           -> (Map.Map b Float, Map.Map b Float, Map.Map b b, Set.Set b)
           -> b
           -> (Map.Map b Float, Map.Map b Float, Map.Map b b, Set.Set b)
updatePath goal current closed s@(g, f, p, o) n =
    if Set.member n closed
    then s
    else
        case Map.lookup current g of
             Just tg -> let tg' = tg + guessLength n current in
                        if tg' < tg || Set.notMember n o
                        then let newPath = Map.insert n current p in
                             let newGs = Map.insert n tg' g in
                             let newFs = Map.insert n (tg' + guessLength n goal) f in
                             let newOp = Set.insert n o in
                             (newGs, newFs, newPath, newOp)
                        else s

             Nothing -> s

