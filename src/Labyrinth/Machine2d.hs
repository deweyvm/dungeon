{-# LANGUAGE ScopedTypeVariables #-}
module Labyrinth.Machine2d((<.>), occuCount, negate, vertStrip, clearBorder) where

import Prelude hiding(foldr, negate)
import Data.Maybe
import Data.Foldable
import Control.Applicative
import Labyrinth.Data.Array2d
import Labyrinth.Util

-- | Apply a list of endomorphisms to an initial value
(<.>) :: Foldable t => a -> t (a -> a) -> a
(<.>) = flip (foldr (.) id)


getOccupants :: Array2d a -> Point -> [a]
getOccupants arr (i, j) =
    extract [ (i + x, j + y) | x <- [-1..1], y <- [-1..1] ]
    where extract = catMaybes . map (geti arr)


countOccupants :: (a -> Bool) -> Array2d a -> Point -> Int
countOccupants f = (count f) .: (getOccupants)

occuCount :: Int -> Array2d Bool -> Array2d Bool
occuCount k arr = (\pt _ -> (countOccupants id arr pt) >= k) <$*> arr

negate :: Array2d Bool -> Array2d Bool
negate = (<$>) not

vertStrip :: Bool -> Int -> Array2d Bool -> Array2d Bool
vertStrip b mods =
    (<$*>) (\(i, j) p -> select p b (modZero i || modZero j))
    where modZero k = k `mod` mods == 0

clearBorder :: Int -> Array2d Bool -> Array2d Bool
clearBorder thick arr@(Array2d cols rows _) =
    (\(i, j) p -> i < thick || j < thick || i > cols - thick - 1 || j > rows - thick - 1 || p) <$*> arr
