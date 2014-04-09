{-# LANGUAGE ScopedTypeVariables #-}
module Labyrinth.Data.Array2d where


import Data.Maybe
import qualified Data.Vector as Vec

data Array2d elt = Array2d Int Int (Vec.Vector elt)

instance Functor Array2d where
    fmap f (Array2d cols rows v) = Array2d cols rows $ Vec.map f v


select :: a -> a -> Bool -> a
select f t p = if p then t else f

unsafeGet :: Array2d a -> Int -> a
unsafeGet (Array2d _ _ v) k = v Vec.! k

get :: Array2d a -> Int -> Int -> Maybe a
get arr@(Array2d cols rows _) i j =
    select (Just $ unsafeGet arr $ fromCoords arr i j)
           Nothing
           (i < 0 || i > cols - 1 || j < 0 || j > rows - 1)

getOrElse :: Array2d a -> a -> Int -> Int -> a
getOrElse arr e x y = fromMaybe e (get arr x y)

toCoords :: Array2d a -> Int -> (Int, Int)
toCoords (Array2d cols _ _) k = (k `mod` cols, k `quot` cols)

fromCoords :: Array2d a -> Int -> Int -> Int
fromCoords (Array2d cols _ _) i j = i + j* cols

imap :: (Int -> Int -> a -> b) -> Array2d a -> Array2d b
imap f arr@(Array2d cols rows v) = Array2d cols rows $ mapped
    where zipped = Vec.zip (Vec.fromList [0..(cols*rows)-1]) v
          mapped = Vec.map (\(k, p) -> let (x, y) = toCoords arr k in f x y p) zipped


zipWithIndex :: Array2d a -> Array2d (Int, Int, a)
zipWithIndex arr = imap (,,) arr


tabulate :: Int -> Int -> a -> (Int -> Int -> a) -> Array2d a
tabulate cols rows initial f =
    imap (\x y _ -> f x y) base
    where base = Array2d cols rows $ Vec.replicate (cols*rows) initial


