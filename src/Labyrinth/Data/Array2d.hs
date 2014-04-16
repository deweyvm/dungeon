{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Labyrinth.Data.Array2d
Description : 2 dimensional arrays
Copyright   : (c) deweyvm 2014
License     : MIT
Maintainer  : deweyvm
Stability   : experimental
Portability : unknown

2 dimensional arrays (implemented as a flat Data.Vector) and utility methods.
-}
module Labyrinth.Data.Array2d(
    Array2d(..),
    geti,
    zipWithIndex,
    tabulate,
    find,
    foldli,
    getOrElse,
    (<$*>)
) where

import Prelude hiding (all)
import Control.Applicative
import Data.Maybe
import qualified Data.Vector as Vec
import Labyrinth.Util

data Array2d elt = Array2d Int Int (Vec.Vector elt)

instance Functor Array2d where
    fmap f (Array2d cols rows v) = Array2d cols rows $ f <$> v

--private
toVec :: Array2d a -> Vec.Vector a
toVec (Array2d _ _ v) = v

--private
toCoords :: Array2d a -> Int -> Point
toCoords (Array2d cols _ _) k = (k `mod` cols, k `quot` cols)

--private
fromCoords :: Array2d a -> Point -> Int
fromCoords (Array2d cols _ _) (i, j) = i + j * cols

--private
unsafeGet :: Array2d a -> Int -> a
unsafeGet (Array2d _ _ v) k = v Vec.! k

--private
get :: Array2d a -> Int -> Int -> Maybe a
get arr@(Array2d cols rows _) i j =
    select (Just $ unsafeGet arr $ fromCoords arr (i, j))
           Nothing
           (i < 0 || i > cols - 1 || j < 0 || j > rows - 1)

-- | Get an element by coordinates or Nothing if the index is out of range
geti :: Array2d a -> Point -> Maybe a
geti a = uncurry $ get a

getOrElse :: Array2d a -> a -> Int -> Int -> a
getOrElse arr e x y = fromMaybe e (get arr x y)

-- | fmap with index
(<$*>) :: (Point -> a -> b) -> Array2d a -> Array2d b
f <$*> arr@(Array2d cols rows v) = Array2d cols rows $ mapped
    where zipped = Vec.zip (Vec.fromList [0..(cols*rows)-1]) v
          mapped = (\(k, p) -> let (x, y) = toCoords arr k in f (x, y) p) <$> zipped

-- | Zips each element of the array with its index
zipWithIndex :: Array2d a -> Array2d (Point, a)
zipWithIndex arr = (,) <$*> arr

-- | Fold left with index
foldli :: (a -> (Point, b) -> a) -> a -> Array2d b -> a
foldli f x arr = Vec.foldl f x $ (toVec . zipWithIndex) arr

-- | Finds the index and element satisfying a predicate
find :: (a -> Bool) -> Array2d a -> Maybe (Point, a)
find f arr =
    let (Array2d _ _ zipped) = zipWithIndex arr in
    Vec.find (\(_, e) -> f e) zipped

-- | Creates a new Array2d from a generating function
tabulate :: Int -> Int -> a -> (Point -> a) -> Array2d a
tabulate cols rows initial f =
    (\p _ -> f p) <$*>  base
    where base = Array2d cols rows $ Vec.replicate (cols*rows) initial


