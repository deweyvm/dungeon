{-# LANGUAGE ScopedTypeVariables #-}
module Labyrinth.Data.Array2d where

import Prelude hiding (all)
import Data.Foldable(all)
import Control.Monad
import Control.Applicative
import Data.Maybe
import qualified Data.Vector as Vec
import Labyrinth.Util

type Point = (Int,Int)

data Array2d elt = Array2d Int Int (Vec.Vector elt)

instance Functor Array2d where
    fmap f (Array2d cols rows v) = Array2d cols rows $ f <$> v

--private
toVec :: Array2d a -> Vec.Vector a
toVec (Array2d _ _ v) = v

toCoords :: Array2d a -> Int -> Point
toCoords (Array2d cols _ _) k = (k `mod` cols, k `quot` cols)

fromCoords :: Array2d a -> Point -> Int
fromCoords (Array2d cols _ _) (i, j) = i + j * cols

(//) :: forall a. Vec.Vector a -> [(Int, a)] -> Vec.Vector a
(//) = (Vec.//)


unsafeGet :: Array2d a -> Int -> a
unsafeGet (Array2d _ _ v) k = v Vec.! k

update :: Array2d a -> Int -> Int -> a -> Array2d a
update arr@(Array2d cols rows v) i j x = Array2d cols rows (v // [(coord, x)])
    where coord = fromCoords arr (i, j)

get :: Array2d a -> Int -> Int -> Maybe a
get arr@(Array2d cols rows _) i j =
    select (Just $ unsafeGet arr $ fromCoords arr (i, j))
           Nothing
           (i < 0 || i > cols - 1 || j < 0 || j > rows - 1)

geti :: Array2d a -> Point -> Maybe a
geti a = uncurry $ get a

getf :: (a -> Bool) -> Array2d a -> Int -> Int -> Maybe a
getf f = mfilter f .:: get

getif :: (a -> Bool) -> Array2d a -> Point -> Maybe a
getif f  = mfilter f .: geti

getis :: (a -> Bool) -> Array2d a -> Point -> Bool
getis f arr pt = all f $ geti arr pt

getOrElse :: Array2d a -> a -> Int -> Int -> a
getOrElse arr e x y = fromMaybe e (get arr x y)


(<$*>) :: (Point -> a -> b) -> Array2d a -> Array2d b
f <$*> arr@(Array2d cols rows v) = Array2d cols rows $ mapped
    where zipped = Vec.zip (Vec.fromList [0..(cols*rows)-1]) v
          mapped = (\(k, p) -> let (x, y) = toCoords arr k in f (x, y) p) <$> zipped

zipWithIndex :: Array2d a -> Array2d (Point, a)
zipWithIndex arr = (,) <$*> arr

foldli :: (a -> (Point, b) -> a) -> a -> Array2d b -> a
foldli f x arr = Vec.foldl f x $ (toVec . zipWithIndex) arr

finda :: (a -> Bool) -> Array2d a -> Maybe (Point, a)
finda f arr =
    let (Array2d _ _ zipped) = zipWithIndex arr in
    Vec.find (\(_, e) -> f e) zipped

tabulate :: Int -> Int -> a -> (Point -> a) -> Array2d a
tabulate cols rows initial f =
    (\p _ -> f p) <$*>  base
    where base = Array2d cols rows $ Vec.replicate (cols*rows) initial


