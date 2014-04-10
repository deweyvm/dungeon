{-# LANGUAGE ScopedTypeVariables, ViewPatterns #-}
module Labyrinth.Flood(floodFill) where

import qualified Data.Set as Set
import Data.Sequence hiding (take, zip, filter)
import Data.Maybe
import Labyrinth.Data.Array2d
import Control.Applicative
import Control.Monad
import Control.Arrow((***))

data Flood = Flood (Set.Set Point) (Seq Point)

newFlood :: Point -> Flood
newFlood = liftM2 Flood Set.singleton singleton


neighbors :: [Point]
neighbors = [ (x, y) | x <- [-1..1], y <- [-1..1], not (x == 0 && y == 0) ]

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
    let ns = filterPoints arr (\(p, elt) -> f elt && not (Set.member p pts)) (getNeighbors pt) in
    let newQueue = (fromList ns) >< work in
    let newPoints = (pts `Set.union` Set.fromList ns) in
    floodHelper f arr (Flood newPoints newQueue)

floodFill :: Point -> (a -> Bool) -> Array2d a -> Set.Set Point
floodFill pt f arr =
    floodHelper f arr $ newFlood pt
