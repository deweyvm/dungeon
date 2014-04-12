{-# LANGUAGE ScopedTypeVariables, ViewPatterns, BangPatterns #-}
module Labyrinth.Flood(floodFill, floodAll) where

import Prelude hiding (foldl)
import qualified Data.Set as Set
import Data.Sequence hiding (take, zip, filter)
import Data.Maybe
import Control.Applicative
import Control.Monad
import Control.Arrow((***))
import Labyrinth.Data.Array2d

data Flood = Flood (Set.Set Point) (Seq Point)

newFlood :: Point -> Flood
newFlood = liftM2 Flood Set.singleton singleton


neighbors :: [Point]
neighbors = [(-1,0), (0, 1), (0, -1), (1, 0)]--[ (x, y) | x <- [-1..1], y <- [-1..1], not (x == 0 && y == 0) ]

getNeighbors :: Point -> [Point]
getNeighbors (i, j) = map ((i +) *** (j +)) neighbors
-- (\(x, y) -> (i + x, j + y)) === (i +) *** (j +)

filterPoints :: forall a . Array2d a -> ((Point,a) -> Bool) -> [Point] -> [Point]
filterPoints arr f pts =
    let x = zip pts $ (geti arr) <$> pts in
    let y = catMaybes $ pairMaybe <$> x in
    fst <$> (filter f y)
  where pairMaybe :: forall c b . (c, Maybe b) -> Maybe (c, b)
        pairMaybe (x, Just y) = Just (x, y)
        pairMaybe (_, Nothing) = Nothing

floodHelper :: (a -> Bool) -> Array2d a -> Flood -> Set.Set Point
floodHelper _ _ (Flood pts (viewl -> EmptyL)) = pts
floodHelper f arr (Flood pts (viewl -> pt :< work)) =
    floodHelper f arr (Flood newPoints newQueue)
    where newPoints = (pts `Set.union` Set.fromList ns)
          newQueue = (fromList ns) >< work
          ns = filterPoints arr (\(p, elt) -> f elt && not (Set.member p pts)) (getNeighbors pt)

floodFill :: Point -> (a -> Bool) -> Array2d a -> Set.Set Point
floodFill pt f arr =
    floodHelper f arr $ newFlood pt

getSolid :: (a -> Bool) -> Array2d a -> [Point]
getSolid f arr = foldli (\xs (pt, x) -> if f x then (pt:xs) else xs) [] arr

floodAll :: (a -> Bool) -> Array2d a -> [Set.Set Point]
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
