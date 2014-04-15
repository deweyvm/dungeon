{-# LANGUAGE ScopedTypeVariables, ViewPatterns, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module Labyrinth.Path(pfind) where

import Prelude hiding(elem)
import Control.Applicative
import Data.Maybe
import Data.Foldable(elem)
import qualified Data.Map as Map
import qualified Data.PSQueue as Q
import qualified Data.Set as Set
import Labyrinth.Data.Array2d(Array2d,Point,geti)
import qualified Labyrinth.Flood as F

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
                      (Q.singleton start $ guessLength start end)
                      Map.empty
                      end

data Path b = Path (Set.Set b)       --ClosedSet
                   (Q.PSQ b Float)   --OpenSet
                   (Map.Map b Float) --g scoroe
                   (Q.PSQ b Float)   --f score
                   (Map.Map b b)     --PathSoFar
                   b                 --goal

getOrElse :: t -> (t -> Maybe a) -> a -> a
getOrElse x f p =
    case f x of
        Just y -> y
        Nothing -> p

rewindPath :: Ord b => Map.Map b b -> b -> [b] -> [b]
rewindPath path end sofar =
    case Map.lookup end path of
        Just next -> rewindPath path next (end:sofar)
        Nothing -> sofar

unsafeGet :: Ord k => k -> Map.Map k a -> a
unsafeGet x = fromJust . (Map.lookup x)

pathHelper :: forall a b.(Ord b, Metric b, PathGraph a b) => a -> Path b -> Maybe [b]
pathHelper coll (Path closedSet openSet gs fs path goal) =
    let current = Q.findMin openSet in

    if (Q.null openSet) || (isNothing current)
    then Nothing
    else let newOpen = Q.deleteMin openSet in
        (fromJust . processCurrent) <$> current
     where processCurrent :: Q.Binding b Float -> Maybe [b]
           processCurrent b =
               let currentNode = Q.key b in
               if currentNode == goal
               then Just $ rewindPath path goal []
               else let newClosed = Set.insert currentNode closedSet in
                    let (gs', fs', path', open') = foldl (inner currentNode newClosed) (gs, fs, path, openSet) (fst <$> (getNeighbors coll currentNode)) in
                       pathHelper coll (Path newClosed open' gs' fs' path' goal)
           inner current closed s@(gs, fs, path, open) n =
               if Set.member n closed
               then s
               else let tg = unsafeGet current gs in
                    if (not . isJust . (Q.lookup n)) open || tg < unsafeGet n gs
                    then let newPath = Map.insert n current path in
                         let newGs = Map.insert n tg gs in
                         let newFs = Q.insert n (tg + guessLength n goal) fs in
                         let newOpen = if (isJust . (Q.lookup n)) open then open else Q.insert n 1.0 open in
                         (newGs, newFs, newPath, newOpen)
                    else s




