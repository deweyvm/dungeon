{-# LANGUAGE ScopedTypeVariables, ViewPatterns #-}
{-|
Module      : Labyrinth.Flood
Description : flood filling
Copyright   : (c) deweyvm 2014
License     : MIT
Maintainer  : deweyvm
Stability   : experimental
Portability : unknown

Implementation of flood fill for Array2d's.
-}
module Labyrinth.Flood(floodFill, floodAll, getNeighbors8) where

import Prelude hiding (foldl)
import qualified Data.Set as Set
import Data.Sequence hiding (take, zip, filter)
import Data.Maybe
import Control.Applicative
import Control.Monad
import Control.Arrow((***))
import Labyrinth.Data.Array2d
import Labyrinth.Util
data Flood = Flood (Set.Set Point) (Seq Point)

mkFlood :: Point -> Flood
mkFlood = liftM2 Flood Set.singleton singleton


neighbors8 :: [Point]
neighbors8 = [ (x, y) | x <- [-1..1], y <- [-1..1], not (x == 0 && y == 0) ]

-- | Retrieves the 8 neighbors of a 2d point
getNeighbors8 :: Point -> [Point]
getNeighbors8 (i, j) = map ((i +) *** (j +)) neighbors8
-- (\(x, y)-> (i + x, j + y)) === (i +) *** (j +)

filterPoints :: forall a . Array2d a -> ((Point,a) -> Bool) -> [Point] -> [Point]
filterPoints arr f pts =
    let x = zip pts $ (geti arr) <$> pts in
    let y = catMaybes $ pairMaybe <$> x in
    fst <$> (filter f y)
  where pairMaybe :: forall c b . (c, Maybe b) -> Maybe (c, b)
        pairMaybe (x, Just y) = Just (x, y)
        pairMaybe (_, Nothing) = Nothing

floodHelper :: (a -> Bool) -> Array2d a -> Int -> Flood -> Set.Set Point
floodHelper _ _ _ (Flood pts (viewl -> EmptyL)) = pts
floodHelper f arr depth (Flood pts (viewl -> pt :< work)) =
    floodHelper f arr (depth + 1) (Flood newPoints newQueue)
    where newPoints = (pts `Set.union` Set.fromList ns)
          newQueue = (fromList ns) >< work
          ns = filterPoints arr (\(p, elt) -> f elt && not (Set.member p pts)) (getNeighbors8 pt)

-- | Flood fills starting from a given point
floodFill :: Point         -- ^ the initial seed point
          -> (a -> Bool)   -- ^ whether or not an element is \"solid\"
          -> Array2d a     -- ^ the grid to be flooded
          -> Set.Set Point -- ^ the set of points in the flood fill
floodFill pt f arr =
    floodHelper f arr 0 $ mkFlood pt

getSolid :: (a -> Bool) -> Array2d a -> [Point]
getSolid f arr = foldli (\xs (pt, x) -> if f x then (pt:xs) else xs) [] arr


-- | Flood fills all regions in a given array
floodAll :: (a -> Bool)     -- ^ whether or not an element is \"solid\"
         -> Array2d a       -- ^ the grid to be flooded
         -> [Set.Set Point] -- ^ the resulting flooded regions
floodAll f arr = floodAllHelper f arr (Set.fromList (getSolid f arr)) []

floodAllHelper :: (a -> Bool)
               -> Array2d a
               -> Set.Set Point   --solid points remaining
               -> [Set.Set Point] --current collection of regions
               -> [Set.Set Point]
floodAllHelper f arr pts sofar =
    case Set.minView pts of
        Just (x, _) -> let filled = floodFill x f arr in
                       let pointsLeft = Set.difference pts filled in
                       floodAllHelper f arr pointsLeft (filled:sofar)
        Nothing -> sofar
