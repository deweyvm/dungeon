{-# LANGUAGE ScopedTypeVariables, ViewPatterns, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module Labyrinth.Path(pfind) where

import Prelude hiding(elem, all)
import Control.Applicative
import Data.Maybe
import qualified Data.Map as Map
import Data.Foldable(all)
import qualified Data.PSQueue as Q
import qualified Data.Set as Set
import Labyrinth.Data.Array2d(Array2d,Point,geti,zipWithIndex)
import qualified Labyrinth.Flood as F
import Labyrinth.Util
import Debug.Trace


class PathGraph a b where
    getNeighbors :: a -> b -> [(b, Float)]

class Metric a where
    guessLength :: a -> a -> Float

class Solid a where
    isSolid :: a -> Bool

instance Solid Bool where
    isSolid = id

instance Solid a => PathGraph (Array2d a) Point where
    getNeighbors arr pt = zip (fst <$> filtered) (repeat 1.0)
        where filtered = filter (isSolid . snd) ns
              ns = catMaybes $ (geti (zipWithIndex arr)) <$> F.getNeighbors pt

instance Metric Point where
    guessLength (i, j) (x, y) = sqrt (xx + yy)
        where xx :: Float = sq (x - i)
              yy :: Float = sq (y - j)
              sq = (** 2) . fromIntegral
--f x = g x + h x
--g x = cost so far
--h x = guess cost of the path
pfind :: forall a b.(Ord b, Metric b, PathGraph a b) => a -> b -> b -> Either String [b]
pfind graph start end = pathHelper graph $ path
    where path :: Path b
          path = Path Set.empty
                      (Q.singleton start 0)
                      Map.empty
                      (Map.singleton start $ guessLength start end)
                      Map.empty
                      end

data Path b = Path (Set.Set b)       --ClosedSet
                   (Q.PSQ b Float)   --OpenSet
                   (Map.Map b Float) --g scoroe
                   (Map.Map b Float)   --f score
                   (Map.Map b b)     --PathSoFar
                   b                 --goal

rewindPath :: Ord b => Map.Map b b -> b -> [b] -> [b]
rewindPath path end sofar =
    case Map.lookup end path of
        Just next -> rewindPath path next (end:sofar)
        Nothing -> sofar

queueContains :: (Ord p, Ord k) => k -> Q.PSQ k p -> Bool
queueContains = isJust .: Q.lookup

pathHelper :: forall a b.(Ord b, Metric b, PathGraph a b) => a -> Path b -> Either String [b]
pathHelper coll (Path closedSet openSet gs fs path goal) =
    case Q.minView openSet of
        Just (current, newOpen) -> trace "test" $ processCurrent current newOpen
        Nothing -> Left "Found no path"
    where processCurrent :: Q.Binding b Float -> Q.PSQ b Float -> Either String [b]
          processCurrent b open =
              let currentNode = Q.key b in
              let newClosed = Set.insert currentNode closedSet in
              if currentNode == goal
              then Right $ rewindPath path goal []
              else let (gs', fs', path', open') = foldl (updatePath goal currentNode newClosed) (gs, fs, path, open) (fst <$> (getNeighbors coll currentNode)) in
                       pathHelper coll (Path newClosed open' gs' fs' path' goal)


updatePath :: (Ord b, Metric b)
           => b
           -> b
           -> Set.Set b
           -> (Map.Map b Float, Map.Map b Float, Map.Map b b, Q.PSQ b Float)
           -> b
           -> (Map.Map b Float, Map.Map b Float, Map.Map b b, Q.PSQ b Float)
updatePath goal current closed s@(g, f, p, o) n =
    if Set.member n closed || (not $ queueContains n o)
    then s
    else
        case Map.lookup current g of
             Just tg -> let tg' = tg + guessLength n current in
                        if tg' < tg
                        then let newPath = Map.insert n current p in
                             let newGs = Map.insert n tg' g in
                             let newFs = Map.insert n (tg' + guessLength n goal) f in
                             let newOp = if queueContains n o then o else Q.insert n 1.0 o in
                             (newGs, newFs, newPath, newOp)
                        else s

             Nothing -> s

