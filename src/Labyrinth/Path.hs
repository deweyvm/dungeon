{-# LANGUAGE ScopedTypeVariables, ViewPatterns, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module Labyrinth.Path(pfind) where

import Prelude hiding(elem)
import Control.Applicative
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.PSQueue as Q
import qualified Data.Set as Set
import Labyrinth.Data.Array2d(Array2d,Point,geti)
import qualified Labyrinth.Flood as F
import Labyrinth.Util

class PathGraph a b | a -> b where
    getNeighbors :: a -> b -> [(b, Float)]

class Metric b where
    guessLength :: b -> b -> Float


instance PathGraph (Array2d Point) Point where
    getNeighbors arr pt = zip ns (repeat 1)
        where ns = catMaybes $ (geti arr) <$> F.getNeighbors pt

instance Metric Point where
    guessLength (i, j) (x, y) = sqrt (xx + yy)
        where xx :: Float = sq (x - i)
              yy :: Float = sq (y - j)
              sq = (** 2) . fromIntegral
--f x = g x + h x
--g x = cost so far
--h x = guess cost of the path
pfind :: forall a b.(Ord b, Metric b, PathGraph a b) => a -> b -> b -> Maybe [b]
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

unsafeGet :: Ord k => k -> Map.Map k a -> a
unsafeGet x = fromJust . (Map.lookup x)

queueContains :: (Ord p, Ord k) => k -> Q.PSQ k p -> Bool
queueContains = isJust .: Q.lookup

pathHelper :: forall a b.(Ord b, Metric b, PathGraph a b) => a -> Path b -> Maybe [b]
pathHelper coll (Path closedSet openSet gs fs path goal) =
    let current = Q.findMin openSet in
    let newOpen = Q.deleteMin openSet in
    let processCurrent :: Q.Binding b Float -> Maybe [b]
        processCurrent b =
            let currentNode = Q.key b in
            if currentNode == goal
            then Just $ rewindPath path goal []
            else let newClosed = Set.insert currentNode closedSet in
                 let (gs', fs', path', open') = foldl (updatePath goal currentNode newClosed) (gs, fs, path, newOpen) (fst <$> (getNeighbors coll currentNode)) in
                     pathHelper coll (Path newClosed open' gs' fs' path' goal) in
    if (Q.null openSet) || (isNothing current)
    then Nothing
    else (fromJust . processCurrent) <$> current



updatePath :: (Ord b, Metric b)
           => b
           -> b
           -> Set.Set b
           -> (Map.Map b Float, Map.Map b Float, Map.Map b b, Q.PSQ b Float)
           -> b
           -> (Map.Map b Float, Map.Map b Float, Map.Map b b, Q.PSQ b Float)
updatePath goal current closed s@(g, f, p, o) n =
    if Set.member n closed
    then s
    else let tg = unsafeGet current g in
         if not $ queueContains n o || tg < unsafeGet n g
         then let newPath = Map.insert n current p in
              let newGs = Map.insert n tg g in
              let newFs = Map.insert n (tg + guessLength n goal) f in
              let newOp = if queueContains n o then o else Q.insert n 1.0 o in
              (newGs, newFs, newPath, newOp)
         else s

